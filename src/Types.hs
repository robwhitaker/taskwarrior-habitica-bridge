{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import GHC.Generics
import qualified Data.UUID as UUID
import qualified Control.Newtype.Generics as NT
import Control.Newtype.Generics (Newtype)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Time.Format as Time
import Data.Time (UTCTime)
import Network.HTTP.Req (HttpException)
import qualified Data.HashMap.Strict as HM

import Data.Aeson
import Data.Aeson.Types

import Data.Maybe (fromJust)

{- TASK TYPE -}

data Task = Task
    { taskHabiticaId :: Maybe UUID
    , taskType :: TaskType
    , taskText :: Text
    , taskDifficulty :: TaskDifficulty
    , taskStatus :: TaskStatus
    , taskDue :: Maybe UTCTime
    , taskModified :: UTCTime
    , rawJson :: Value
    } deriving (Show)

instance Eq Task where
    -- The Eq instance ignores the modified field as *when* the tasks changed
    -- is inconsequential to comparing the *content* of the task.
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

data TaskStatus
    = Pending
    | Completed
    | Deleted
  deriving (Show, Eq)

data TaskType
    = Habit
    | Daily
    | Todo
    | Reward
  deriving (Show, Eq)

instance FromJSON TaskType where
    parseJSON = withText "task type" $ \s ->
        case s of
            "habit" -> return Habit
            "daily" -> return Daily
            "todo" -> return Todo
            "reward" -> return Reward
            _ -> fail "Invalid string provided as task type."

instance ToJSON TaskType where
    toJSON = String . T.toLower . T.pack . show

newtype UUID = UUID UUID.UUID
    deriving (Show, Eq, Generic)
instance Newtype UUID

instance FromJSON UUID where
    parseJSON = withText "UUID" $
        maybe (fail "Invalid UUID.") (return . NT.pack) . UUID.fromText

instance ToJSON UUID where
    toJSON (UUID uuid) =
        String $ UUID.toText uuid

unsafeUUID :: Text -> UUID
unsafeUUID uuidStr =
    UUID $ fromJust $ UUID.fromText uuidStr

{- SPECIALIZED TASK TYPES
--
-- For encoding and decoding between Taskwarrior and Habitica
-}

newtype HabiticaTask = HabiticaTask Task
    deriving (Show, Eq, Generic)
instance Newtype HabiticaTask

instance FromJSON HabiticaTask where
    parseJSON = withObject "habitica task" $ \o -> fmap NT.pack $ Task
            <$> (fmap Just (o .: "id"))
            <*> (o .: "type")
            <*> (o .: "text")
            <*> (o .: "priority" >>= \n ->
                    case n :: Double of
                        0.1 -> return Trivial
                        1 -> return Easy
                        1.5 -> return Medium
                        2 -> return Hard
                        _ -> fail "Invalid number provided as task difficulty (priority)."
                )
            <*> (o .: "completed" >>= \completed -> return $
                    if completed then Completed else Pending)
            <*> (o .:? "date" >>= text2Time)
            <*> (o .: "updatedAt" >>= textToTime habiticaTimeFormat)
            <*> return (Object o)
      where
        -- Because sometimes Habitica likes to represent "no time" as
        -- empty string.
        text2Time Nothing = return Nothing
        text2Time (Just "") = return Nothing
        text2Time (Just time) = Just <$> textToTime habiticaTimeFormat time

instance ToJSON HabiticaTask where
    toJSON (HabiticaTask task) =
        object
            [ "text" .= taskText task
            , "type" .= taskType task
            , "date" .= (timeToText habiticaTimeFormat <$> taskDue task)
            , "priority" .= case taskDifficulty task of
                    Trivial -> Number 0.1
                    Easy -> Number 1
                    Medium -> Number 1.5
                    Hard -> Number 2
            ]

newtype TaskwarriorTask = TaskwarriorTask Task
    deriving (Show, Eq, Generic)
instance Newtype TaskwarriorTask

instance FromJSON TaskwarriorTask where
    parseJSON = withObject "taskwarrior task" $ \o -> fmap NT.pack $ Task
        <$> (o .:? "habitica_uuid")
        <*> (o .:? "habitica_task_type" .!= Todo)
        <*> (o .: "description")
        <*> (o .:? "habitica_difficulty" >>= \difficulty ->
                case difficulty :: Maybe Text of
                    Nothing -> return Easy
                    Just "trivial" -> return Trivial
                    Just "easy" -> return Easy
                    Just "medium" -> return Medium
                    Just "hard" -> return Hard
                    _ -> fail "Invalid string provided as task difficulty (priority)."
            )
        <*> (o .: "status" >>= \status ->
                case status :: Text of
                    "pending" -> return Pending :: Parser TaskStatus
                    "completed" -> return Completed
                    "deleted" -> return Deleted
                    _ -> fail "Invalid status."
            )
        <*> (o .:? "due" >>= maybe (return Nothing) (fmap Just . textToTime taskwarriorTimeFormat))
        <*> (o .: "modified" >>= textToTime taskwarriorTimeFormat)
        <*> return (Object o)

-- TOJSON difficulty:
instance ToJSON TaskwarriorTask where
    toJSON (TaskwarriorTask task) =
        -- Taskwarrior's "import" functionality removes all fields that
        -- are not included in the JSON, so by filtering out Null, we
        -- effectively remove the field
        Object $ HM.filter (/= Null) (HM.union newObj oldObj)
      where
        (Object newObj) =
            object
                [ "description" .= taskText task
                , "habitica_task_type" .= taskType task
                , "habitica_uuid" .= taskHabiticaId task
                , "habitica_difficulty" .= (String . T.toLower . T.pack . show . taskDifficulty) task
                , "status" .= (String . T.toLower . T.pack . show . taskStatus) task
                , "due" .= (timeToText taskwarriorTimeFormat <$> taskDue task)
                ]
        -- TODO: Is there a safer way to do this? This will fail
        --       if the Value isn't an Object, though that can only
        --       happen if Tasks are constructed manually.
        (Object oldObj) =
            rawJson task

-- These don't really belong here, but they are needed for encoding/decoding time
habiticaTimeFormat :: String
habiticaTimeFormat =
    "%0Y-%m-%dT%T%QZ"

taskwarriorTimeFormat :: String
taskwarriorTimeFormat =
    "%0Y%m%dT%H%M%SZ"

textToTime :: String -> Text -> Parser UTCTime
textToTime timeFormat txt =
    maybe (fail $ "Unable to parse time: " <> T.unpack txt) return $ parseUTCTime timeFormat txt

timeToText :: String -> UTCTime -> Text
timeToText timeFormat time =
    T.pack $ Time.formatTime Time.defaultTimeLocale timeFormat time

parseUTCTime :: String -> Text -> Maybe UTCTime
parseUTCTime format =
    Time.parseTimeM True Time.defaultTimeLocale format . T.unpack

{- REQUEST TYPES
-
-  Types for working with web requests
-}

data HabiticaResponse a
    = DataResponse a
    | ErrorResponse { err :: Text, errMessage :: Text }
    | ParseError String
    | HttpException HttpException
  deriving (Show)

instance (FromJSON a) => FromJSON (HabiticaResponse a) where
    parseJSON = withObject "habitica response" $ \o -> do
        success <- o .: "success"
        if success then
            DataResponse <$> o .: "data"
        else
            ErrorResponse
                <$> o .: "error"
                <*> o .: "message"

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
    = UpdateHabitica HabiticaTask HasStatusChange
    | UpdateTaskwarrior TaskwarriorTask
  deriving (Show)
