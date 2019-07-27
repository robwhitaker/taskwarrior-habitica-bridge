{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import           GHC.Generics

import           Control.Applicative       (empty, (<|>))

import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)

import           Control.Newtype.Generics  (Newtype)
import qualified Control.Newtype.Generics  as NT

import qualified Data.HashMap.Strict       as HM
import qualified Data.Maybe                as Maybe
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Time                 (UTCTime)
import qualified Data.Time.Format          as Time
import qualified Data.UUID                 as UUID

import           Data.Aeson                (FromJSON, Object, ToJSON,
                                            Value (..), parseJSON, toJSON,
                                            (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson                as Aeson
import           Data.Aeson.Types          (Parser)

import           Network.HTTP.Req          (HttpException)

-- Task type

data Task status annotations = Task
    { taskHabiticaId  :: Maybe UUID
    , taskType        :: TaskType
    , taskText        :: Text
    , taskNote        :: Text
    , taskDifficulty  :: TaskDifficulty
    , taskStatus      :: status
    , taskDue         :: Maybe UTCTime
    , taskModified    :: UTCTime
    , taskAnnotations :: annotations
    , rawJson         :: Object
    } deriving (Show)

-- The Eq instance ignores the modified field as *when* the tasks changed
-- is inconsequential to comparing the *content* of the task. It also ignores
-- the rawJson as that's only stored for the sake of not losing unparsed fields.
instance (Eq status, Eq annotations) => Eq (Task status annotations) where
    (==) t1 t2 =
        (taskHabiticaId t1 == taskHabiticaId t2) &&
        (taskType t1 == taskType t2) &&
        (taskText t1 == taskText t2) &&
        (taskNote t1 == taskNote t2) &&
        (taskDifficulty t1 == taskDifficulty t2) &&
        (taskStatus t1 == taskStatus t2) &&
        (taskDue t1 == taskDue t2) &&
        (taskAnnotations t1 == taskAnnotations t2)

data TaskDifficulty
    = Trivial
    | Easy
    | Medium
    | Hard
  deriving (Show, Eq)

data TWTaskStatus
    = TWPending
    | TWWaiting
    | TWRecurring
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
        Aeson.withText "task type" $ \s ->
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
        Aeson.withText "UUID" $
            maybe (fail "Invalid UUID.") (return . NT.pack) . UUID.fromText

instance ToJSON UUID where
    toJSON (UUID uuid) = String $ UUID.toText uuid

-- Specialized task types for encoding and decoding between
-- Taskwarrior and Habitica

newtype HabiticaTask =
    HabiticaTask (Task HTaskStatus ())
  deriving (Show, Eq, Generic)

instance Newtype HabiticaTask

instance FromJSON HabiticaTask where
    parseJSON =
        Aeson.withObject "habitica task" $ \o ->
            fmap NT.pack $ do
                hId <- Just <$> (o .: "id")
                type_ <- o .: "type"
                text <- o .: "text"
                note <- o .: "notes"
                priority <-
                    o .: "priority" >>= \n ->
                        case n :: Double of
                            0.1 -> return Trivial
                            1 -> return Easy
                            1.5 -> return Medium
                            2 -> return Hard
                            _ -> fail "Invalid number provided as task difficulty (priority)."
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
                return $ Task hId type_ text note priority status due modified () o
      where
        text2Time Nothing     = return Nothing
        -- Because sometimes Habitica likes to represent "no time" as
        -- empty string.
        text2Time (Just "")   = return Nothing
        text2Time (Just time) = Just <$> textToTime habiticaTimeFormat time

instance ToJSON HabiticaTask where
    toJSON (HabiticaTask task) =
        Aeson.object
            [ "text" .= taskText task
            , "notes" .= taskNote task
            , "type" .= taskType task
            , "date" .= (timeToText habiticaTimeFormat <$> taskDue task)
            , "priority" .=
              case taskDifficulty task of
                  Trivial -> Number 0.1
                  Easy    -> Number 1
                  Medium  -> Number 1.5
                  Hard    -> Number 2
            ]

data Annotation = Annotation
    { annoEntry       :: Text -- technically a date, but we don't care about it
    , annoDescription :: Text
    }
  deriving (Show, Eq)

instance FromJSON Annotation where
    parseJSON =
        Aeson.withObject "annotation" $ \o ->
            Annotation
                <$> (o .: "entry")
                <*> (o .: "description")

instance ToJSON Annotation where
    toJSON annotation =
        Aeson.object
            [ "entry" .= annoEntry annotation
            , "description" .= annoDescription annotation
            ]

newtype TaskwarriorTask =
    TaskwarriorTask (Task TWTaskStatus [Annotation])
  deriving (Show, Eq, Generic)

instance Newtype TaskwarriorTask

newtype PartialTaskwarriorTask =
    PartialTaskwarriorTask (UUID, Text -> TaskwarriorTask)

instance FromJSON PartialTaskwarriorTask where
    parseJSON =
        Aeson.withObject "taskwarrior task" $ \o -> do
            hId <- o .:? "habitica_uuid"
            type' <- o .:? "habitica_task_type" .!= Todo
            text <- o .: "description"
            difficulty <- o .:? "habitica_difficulty" >>= \difficulty ->
                case difficulty :: Maybe Text of
                    Nothing -> return Easy
                    Just "trivial" -> return Trivial
                    Just "easy" -> return Easy
                    Just "medium" -> return Medium
                    Just "hard" -> return Hard
                    _ -> fail "Invalid string provided as task difficulty (priority)."
            status <- o .: "status" >>= \status ->
                case status :: Text of
                    "pending"   -> return TWPending :: Parser TWTaskStatus
                    "waiting"   -> return TWWaiting
                    "completed" -> return TWCompleted
                    "deleted"   -> return TWDeleted
                    "recurring" -> return TWRecurring
                    _           -> fail "Invalid status."
            due <- o .:? "due" >>=
                maybe (return Nothing) (fmap Just . textToTime taskwarriorTimeFormat)
            modified <- o .: "modified" >>= textToTime taskwarriorTimeFormat
            annotations <- o .:? "annotations" >>=
                maybe (return []) (traverse parseJSON)
            let
                completeTask note =
                    TaskwarriorTask $
                        Task hId type' text note difficulty status due modified annotations o
            fmap PartialTaskwarriorTask $ (,)
                <$> (o .: "uuid")
                <*> return completeTask

instance ToJSON TaskwarriorTask where
    -- Taskwarrior's "import" functionality removes all fields that
    -- are not included in the JSON, so by filtering out Null, we
    -- effectively remove the field
    toJSON (TaskwarriorTask task) = Object $ HM.filter (/= Null) (HM.union newObj oldObj)
      where
        newObj =
            HM.fromList
                [ "description" .= taskText task
                , "habitica_task_type" .= taskType task
                , "habitica_uuid" .= taskHabiticaId task
                , "habitica_difficulty" .= (String . T.toLower . T.pack . show . taskDifficulty) task
                , "status" .= (String . T.drop 2 . T.toLower . T.pack . show . taskStatus) task
                , "due" .= (timeToText taskwarriorTimeFormat <$> taskDue task)
                , "annotations" .= taskAnnotations task
                ]
        oldObj = rawJson task

data HabiticaUserStats = HabiticaUserStats
    { statsHp  :: Double
    , statsMp  :: Double
    , statsExp :: Double
    , statsGp  :: Double
    , statsLvl :: Int
    } deriving (Show)

instance FromJSON HabiticaUserStats where
    parseJSON = Aeson.withObject "Habitica user stats" $ \o -> do
        stats <- Maybe.fromMaybe o <$> o .:? "stats"
        HabiticaUserStats
            <$> stats .: "hp"
            <*> stats .: "mp"
            <*> stats .: "exp"
            <*> stats .: "gp"
            <*> stats .: "lvl"

instance ToJSON HabiticaUserStats where
    toJSON stats =
        Aeson.object
            [ "hp" .= statsHp stats
            , "mp" .= statsMp stats
            , "exp" .= statsExp stats
            , "gp" .= statsGp stats
            , "lvl" .= statsLvl stats
            ]


newtype ItemDrop =
    ItemDrop Text
  deriving (Show)

instance FromJSON ItemDrop where
    parseJSON = Aeson.withObject "item drop" $ \o -> do
        dialog <- runMaybeT $
            maybeField "_tmp" o
                >>= maybeField "drop"
                >>= maybeField "dialog"
        maybe empty (return . ItemDrop) dialog
      where
        maybeField txt obj = MaybeT (obj .:? txt)

-- Encoding and decoding time

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

-- Web request types

data ResponseData a = ResponseData
    { resBody      :: a
    , resUserStats :: Maybe HabiticaUserStats
    , resItemDrop  :: Maybe ItemDrop
    } deriving (Show)

instance Functor ResponseData where
    fmap f resData = resData { resBody = f (resBody resData) }

data HabiticaResponse a
    = DataResponse (ResponseData a)
    | ErrorResponse { errKey     :: Text
                    , errMessage :: Text
                    }
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
        Aeson.withObject "habitica response" $ \o -> do
            success <- o .: "success"
            if success
                then fmap DataResponse $ ResponseData
                    <$> o .: "data"
                    <*> o `maybeParse` "data"
                    <*> o `maybeParse` "data"
                else ErrorResponse <$> o .: "error" <*> o .: "message"
      where
        maybeParse :: FromJSON a => Object -> Text -> Parser (Maybe a)
        maybeParse obj txt =
            obj .: txt >>= \inJson -> parseJSON inJson <|> return Nothing

-- Error types
-- In this case, errors are just strings to give the user on failure.

type Error = String
