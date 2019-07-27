{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}

module App
    (
    -- * Types
      App
    , AppError
    , AppReader
    , Args(..)
    , Cmd(..)
    , Env(..)

    -- * Running App
    , runProgram

    -- * Helper functions
    , getEnvVar
    , setEnvVar
    , decodeTaskwarriorJSON
    ) where

import           Control.Monad          (void)
import           Control.Monad.Except   (ExceptT, MonadError, liftEither,
                                         runExceptT, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, asks, runReaderT)

import qualified System.Directory       as Dir
import qualified System.Environment     as Environment
import           System.FilePath        ((<.>), (</>))

import qualified Data.Aeson             as Aeson
import qualified Data.String            as String
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.UUID              as UUID

import           Taskwarrior            (Taskwarrior (..))
import qualified Taskwarrior
import           Types                  (Error, PartialTaskwarriorTask (..),
                                         TaskwarriorTask, UUID (..))
import           Web                    (HabiticaHeaders)
import qualified Web

-- Constants

-- Minimum Taskwarrior version (for proper "import" functionality)
minTaskwarriorVersion :: String
minTaskwarriorVersion = "2.5.0"

envVarName :: String
envVarName = "TASK2HABITICA_RUNNING"

-- App monad

newtype App a = App { unApp :: ReaderT Env (ExceptT String IO) a }
    deriving ( Functor, Applicative, Monad
             , AppReader, AppError, MonadIO
             )

runApp :: Env -> App a -> IO (Either Error a)
runApp env app =
    runExceptT $ runReaderT (unApp app) env

runProgram :: (Error -> IO ()) -> (Cmd -> App ()) -> IO ()
runProgram errorHandler app = do
    input <- runExceptT $ do
        (cmd, args) <- getArguments
        env <- getEnvironment args
        return (cmd, env)
    either
        errorHandler
        (\(cmd, env) -> void $ runApp env (app cmd))
        input

-- Commands the app accepts

data Cmd
    = Add
    | Modify
    | Exit
    | Sync

-- Type aliases for our app

type AppReader = MonadReader Env

type AppError = MonadError Error

-- Environment and args

data Env = Env
    { envArgs                   :: Args
    , envTaskwarrior            :: Taskwarrior
    , envTaskNoteDir            :: FilePath
    , envTaskNotePrefix         :: Text
    , envTaskNoteExtension      :: FilePath
    , envHabiticaHeaders        :: HabiticaHeaders
    , envHabiticaUserStatsCache :: FilePath
    }

newtype Args = Args
    { argsVerbose :: Bool
    } deriving (Show)

getEnvironment :: (MonadIO m, AppError m) => Args -> m Env
getEnvironment args = do
    -- An incomplete instance of Taskwarrior which always reads task
    -- notes as empty string. This instance only exists to get access
    -- to taskGet for reading the config file.
    -- TODO: Make a different type for this
    incompleteTaskwarrior <-
        Taskwarrior.requireTaskwarrior
            minTaskwarriorVersion
            (\(PartialTaskwarriorTask (_, f)) -> return $ f "")
    taskNoteDir <- getTaskNoteDir incompleteTaskwarrior
    taskNoteExtension <- getRCWithDefault incompleteTaskwarrior ".txt" "rc.tasknote.extension"
    taskNotePrefix <-
        T.pack <$> getRCWithDefault incompleteTaskwarrior "[tasknote]" "rc.tasknote.prefix"
    taskwarrior <-
        Taskwarrior.requireTaskwarrior
            minTaskwarriorVersion
            (completePartialTaskwarriorTask taskNoteExtension taskNoteDir)
    headers <- getHabiticaHeaders taskwarrior
    userStatsCache <- getStatsCacheFilePath taskwarrior
    return $
        Env
            args
            taskwarrior
            taskNoteDir
            taskNotePrefix
            taskNoteExtension
            headers
            userStatsCache

getArguments :: (MonadIO m, AppError m) => m (Cmd, Args)
getArguments = do
    args <- liftIO Environment.getArgs
    cmd <-
        case safeHead args of
            Nothing -> throwError "No command provided."
            Just "add" -> return Add
            Just "modify" -> return Modify
            Just "exit" -> return Exit
            Just "sync" -> return Sync
            Just other -> throwError $ "Invalid command provided: " <> other

    let parsedArgs = foldl (\acc arg ->
            case arg of
                "--verbose" -> acc{ argsVerbose = True }
                _           -> acc
            ) (Args False) (safeTail args)

    return (cmd, parsedArgs)
  where
    safeHead (x:_) = Just x
    safeHead []    = Nothing

    safeTail (_:rest) = rest
    safeTail []       = []

getRCWithDefault :: MonadIO m => Taskwarrior -> String -> String -> m String
getRCWithDefault taskwarrior def field = liftIO $ do
    let Taskwarrior{taskGet} = taskwarrior
    rawField <- taskGet field
    if T.null (T.strip (T.pack rawField))
        then return def
        else return rawField

getStatsCacheFilePath :: MonadIO m => Taskwarrior -> m FilePath
getStatsCacheFilePath taskwarrior = liftIO $ do
    let Taskwarrior{taskGet} = taskwarrior
    dataDir <- taskGet "rc.data.location"
    fullPath <- expandPath dataDir
    return (fullPath </> "cached_habitica_stats.json")

getTaskNoteDir :: MonadIO m => Taskwarrior -> m FilePath
getTaskNoteDir taskwarrior = liftIO $ do
    dataDir <- getRCWithDefault taskwarrior "~/.task/notes/" "rc.tasknote.location"
    expandPath dataDir

expandPath :: FilePath -> IO FilePath
expandPath fp =
    if take 1 fp == "~"
        then do home <- Dir.getHomeDirectory
                return $ home </> dropWhile (=='/') (drop 1 fp)
        else return fp

getHabiticaHeaders :: (MonadIO m, MonadError String m) => Taskwarrior -> m HabiticaHeaders
getHabiticaHeaders taskwarrior = do
    let Taskwarrior{..} = taskwarrior
    habiticaId <- liftIO $ taskGet "rc.habitica.user_id"
    habiticaApiKey <- liftIO $ taskGet "rc.habitica.api_key"
    case Web.habiticaHeaders (T.pack habiticaId) (T.pack habiticaApiKey) of
        Nothing -> throwError "Missing or malformed Habitica credentials in taskrc."
        Just headers -> return headers

-- Exposed helper functions

getEnvVar :: MonadIO m => m (Maybe String)
getEnvVar =
    liftIO $ Environment.lookupEnv envVarName

setEnvVar :: MonadIO m => m ()
setEnvVar =
    liftIO $ Environment.setEnv envVarName "1"

decodeTaskwarriorJSON :: String -> App TaskwarriorTask
decodeTaskwarriorJSON taskJson = do
    fileExtension <- asks envTaskNoteExtension
    fileDir <- asks envTaskNoteDir
    decodeTaskwarriorJSON' fileExtension fileDir taskJson

-- Helper functions

completePartialTaskwarriorTask
    :: (MonadError Error m, MonadIO m)
    => FilePath -> FilePath -> PartialTaskwarriorTask -> m TaskwarriorTask
completePartialTaskwarriorTask fileExtension fileDir partialTask = do
    let
        PartialTaskwarriorTask (UUID uuid, completeTask) = partialTask
        fileName = T.unpack (UUID.toText uuid)
        fullPath = fileDir </> fileName <.> fileExtension
    noteExists <- liftIO $ Dir.doesFileExist fullPath
    note <-
        if noteExists
            then liftIO $ T.readFile fullPath
            else return ""
    return (completeTask note)

decodeTaskwarriorJSON'
    :: (MonadError Error m, MonadIO m)
    => FilePath -> FilePath -> String -> m TaskwarriorTask
decodeTaskwarriorJSON' fileExtension fileDir taskJson = do
    partialTask <- liftEither $ Aeson.eitherDecode $ String.fromString taskJson
    completePartialTaskwarriorTask fileExtension fileDir partialTask
