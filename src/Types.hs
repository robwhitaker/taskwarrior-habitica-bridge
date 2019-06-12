{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import           GHC.Generics

import           Control.Applicative       (empty, (<|>))

import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)

import           Control.Newtype.Generics  (Newtype)
import qualified Control.Newtype.Generics  as NT

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict       as HM
import           Data.Maybe                (fromJust, fromMaybe, isJust)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Time                 (UTCTime)
import qualified Data.Time.Format          as Time
import qualified Data.UUID                 as UUID

import           Network.HTTP.Req          (HttpException)

{- TASK TYPE -}
data Task status = Task
    { taskHabiticaId :: Maybe UUID
    , taskType       :: TaskType
    , taskText       :: Text
    , taskDifficulty :: TaskDifficulty
    , taskStatus     :: status
    , taskDue        :: Maybe UTCTime
    , taskModified   :: UTCTime
    , rawJson        :: Value
    } deriving (Show)

-- The Eq instance ignores the modified field as *when* the tasks changed
-- is inconsequential to comparing the *content* of the task. It also ignores
-- the rawJson as that's only stored for the sake of not losing unparsed fields.
instance Eq status => Eq (Task status) where
    (==) t1 t2 =
        (taskHabiticaId t1 == taskHabiticaId t2) &&
        (taskType t1 == taskType t2) &&
        (taskText t1 == taskText t2) &&
        (taskDifficulty t1 == taskDifficulty t2) &&
        (taskStatus t1 == taskStatus t2) &&
        (taskDue t1 == taskDue t2)

data TaskDifficulty
    = Trivial
    | Easy
    | Medium
    | Hard
    deriving (Show, Eq)

data TWTaskStatus
    = TWPending
    | TWWaiting
    | TWCompleted
    | TWDeleted
    deriving (Show, Eq)

data HTaskStatus
    = HPending
    | HCompleted
    | HDeleted
    deriving (Show, Eq)

data TaskType
    = Habit
    | Daily
    | Todo
    | Reward
    deriving (Show, Eq)

instance FromJSON TaskType where
    parseJSON =
        withText "task type" $ \s ->
            case s :: Text of
                "habit"  -> return Habit
                "daily"  -> return Daily
                "todo"   -> return Todo
                "reward" -> return Reward
                _        -> fail "Invalid string provided as task type."

instance ToJSON TaskType where
    toJSON = String . T.toLower . T.pack . show

newtype UUID =
    UUID UUID.UUID
    deriving (Show, Eq, Generic)

instance Newtype UUID

instance FromJSON UUID where
    parseJSON =
        withText "UUID" $
        maybe (fail "Invalid UUID.") (return . NT.pack) . UUID.fromText

instance ToJSON UUID where
    toJSON (UUID uuid) = String $ UUID.toText uuid

unsafeUUID :: Text -> UUID
unsafeUUID uuidStr = UUID $ fromJust $ UUID.fromText uuidStr

{- SPECIALIZED TASK TYPES
--
-- For encoding and decoding between Taskwarrior and Habitica
-}
newtype HabiticaTask =
    HabiticaTask (Task HTaskStatus)
  deriving (Show, Eq, Generic)

instance Newtype HabiticaTask

instance FromJSON HabiticaTask where
    parseJSON =
        withObject "habitica task" $ \o ->
            fmap NT.pack $ do
                hId <- Just <$> (o .: "id")
                type_ <- o .: "type"
                text <- o .: "text"
                priority <-
                    o .: "priority" >>= \n ->
                        case n :: Double of
                            0.1 -> return Trivial
                            1 -> return Easy
                            1.5 -> return Medium
                            2 -> return Hard
                            _ ->
                                fail
                                    "Invalid number provided as task difficulty (priority)."
                isCompleted <- o .: "completed"
                status <-
                    if type_ == Daily
                        then do
                            isDue <- o .: "isDue"
                            if not isCompleted && isDue
                                then return HPending
                                else return HCompleted
                        else if isCompleted
                                 then return HCompleted
                                 else return HPending
                due <- o .:? "date" >>= text2Time
                modified <- o .: "updatedAt" >>= textToTime habiticaTimeFormat
                let rawJson = Object o
                return $ Task hId type_ text priority status due modified rawJson
      where
        text2Time Nothing     = return Nothing
        -- Because sometimes Habitica likes to represent "no time" as
        -- empty string.
        text2Time (Just "")   = return Nothing
        text2Time (Just time) = Just <$> textToTime habiticaTimeFormat time

instance ToJSON HabiticaTask where
    toJSON (HabiticaTask task) =
        object
            [ "text" .= taskText task
            , "type" .= taskType task
            , "date" .= (timeToText habiticaTimeFormat <$> taskDue task)
            , "priority" .=
              case taskDifficulty task of
                  Trivial -> Number 0.1
                  Easy    -> Number 1
                  Medium  -> Number 1.5
                  Hard    -> Number 2
            ]

newtype TaskwarriorTask =
    TaskwarriorTask (Task TWTaskStatus)
  deriving (Show, Eq, Generic)

instance Newtype TaskwarriorTask

instance FromJSON TaskwarriorTask where
    parseJSON =
        withObject "taskwarrior task" $ \o ->
            fmap NT.pack $
            Task <$> (o .:? "habitica_uuid") <*> (o .:? "habitica_task_type" .!= Todo) <*>
            (o .: "description") <*>
            (o .:? "habitica_difficulty" >>= \difficulty ->
                 case difficulty :: Maybe Text of
                     Nothing -> return Easy
                     Just "trivial" -> return Trivial
                     Just "easy" -> return Easy
                     Just "medium" -> return Medium
                     Just "hard" -> return Hard
                     _ -> fail "Invalid string provided as task difficulty (priority).") <*>
            (o .: "status" >>= \status ->
                 case status :: Text of
                     "pending"   -> return TWPending :: Parser TWTaskStatus
                     "waiting"   -> return TWWaiting
                     "completed" -> return TWCompleted
                     "deleted"   -> return TWDeleted
                     _           -> fail "Invalid status.") <*>
            (o .:? "due" >>=
             maybe (return Nothing) (fmap Just . textToTime taskwarriorTimeFormat)) <*>
            (o .: "modified" >>= textToTime taskwarriorTimeFormat) <*>
            return (Object o)

instance ToJSON TaskwarriorTask where
    -- Taskwarrior's "import" functionality removes all fields that
    -- are not included in the JSON, so by filtering out Null, we
    -- effectively remove the field
    toJSON (TaskwarriorTask task) = Object $ HM.filter (/= Null) (HM.union newObj oldObj)
      where
        (Object newObj) =
            object
                [ "description" .= taskText task
                , "habitica_task_type" .= taskType task
                , "habitica_uuid" .= taskHabiticaId task
                , "habitica_difficulty" .= (String . T.toLower . T.pack . show . taskDifficulty) task
                , "status" .= (String . T.drop 2 . T.toLower . T.pack . show . taskStatus) task
                , "due" .= (timeToText taskwarriorTimeFormat <$> taskDue task)
                ]
        -- TODO: Is there a safer way to do this? This will fail
        --       if the Value isn't an Object, though that can only
        --       happen if Tasks are constructed manually.
        (Object oldObj) = rawJson task

data HabiticaUserStats = HabiticaUserStats
    { statsHp  :: Double
    , statsMp  :: Double
    , statsExp :: Double
    , statsGp  :: Double
    , statsLvl :: Int
    } deriving (Show)

instance FromJSON HabiticaUserStats where
    parseJSON = withObject "Habitica user stats" $ \o -> do
        stats <- fromMaybe o <$> o .:? "stats"
        HabiticaUserStats <$>
            stats .: "hp" <*>
            stats .: "mp" <*>
            stats .: "exp" <*>
            stats .: "gp" <*>
            stats .: "lvl"

newtype ItemDrop = ItemDrop Text
  deriving (Show)

instance FromJSON ItemDrop where
    parseJSON = withObject "item drop" $ \o -> do
        dialog <- runMaybeT $
            maybeField "_tmp" o
                >>= maybeField "drop"
                >>= maybeField "dialog"
        maybe
            empty
            (return . ItemDrop)
            dialog
      where
        maybeField txt obj = MaybeT (obj .:? txt)

-- These don't really belong here, but they are needed for encoding/decoding time
habiticaTimeFormat :: String
habiticaTimeFormat = "%0Y-%m-%dT%T%QZ"

taskwarriorTimeFormat :: String
taskwarriorTimeFormat = "%0Y%m%dT%H%M%SZ"

textToTime :: String -> Text -> Parser UTCTime
textToTime timeFormat txt =
    maybe (fail $ "Unable to parse time: " <> T.unpack txt) return $
    parseUTCTime timeFormat txt

timeToText :: String -> UTCTime -> Text
timeToText timeFormat time =
    T.pack $ Time.formatTime Time.defaultTimeLocale timeFormat time

parseUTCTime :: String -> Text -> Maybe UTCTime
parseUTCTime format = Time.parseTimeM True Time.defaultTimeLocale format . T.unpack

{- REQUEST TYPES
-
-  Types for working with web requests
-}

data ResponseData a = ResponseData
    { resBody      :: a
    , resUserStats :: Maybe HabiticaUserStats
    , resItemDrop  :: Maybe ItemDrop
    } deriving (Show)

instance Functor ResponseData where
    fmap f resData = resData { resBody = f (resBody resData) }

data HabiticaResponse a
    = DataResponse (ResponseData a)
    | ErrorResponse { err        :: Text
                    , errMessage :: Text }
    | ParseError String
    | HttpException HttpException
    deriving (Show)

instance Functor HabiticaResponse where
    fmap f (DataResponse val)  = DataResponse (fmap f val)
    fmap _ (ErrorResponse e m) = ErrorResponse e m
    fmap _ (ParseError str)    = ParseError str
    fmap _ (HttpException e)   = HttpException e

instance (FromJSON a) => FromJSON (HabiticaResponse a) where
    parseJSON =
        withObject "habitica response" $ \o -> do
            success <- o .: "success"
            if success
                then fmap DataResponse $ ResponseData <$>
                    o .: "data" <*>
                    o `maybeParse` "data" <*>
                    o `maybeParse` "data"
                else ErrorResponse <$> o .: "error" <*> o .: "message"
      where
        maybeParse :: FromJSON a => Object -> Text -> Parser (Maybe a)
        maybeParse obj txt =
            obj .: txt >>= \json -> parseJSON json <|> return Nothing

{- CHANGE TYPES
-
-  Types for representing changes that need to be made
-}
data Change
    = AddToTaskwarrior HabiticaTask
    | DeleteFromTaskwarrior TaskwarriorTask
    | AddToHabitica TaskwarriorTask
    | Update UpdateChange
    deriving (Show)

type HasStatusChange = Bool

data UpdateChange
    = UpdateHabitica HabiticaTask
                     HasStatusChange
    | UpdateTaskwarrior TaskwarriorTask
    deriving (Show)

{- ARG TYPES -}

data SyncArgs = SyncArgs
    { syncVerbose :: Bool }
