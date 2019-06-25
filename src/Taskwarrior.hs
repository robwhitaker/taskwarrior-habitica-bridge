{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Taskwarrior
    (
    -- * Types
      Taskwarrior(..)

    -- * Get the validated Taskwarrior instance
    , requireTaskwarrior
    ) where

import           Control.Monad             (when)
import           Control.Monad.Except      (MonadError, liftEither, throwError)
import           Control.Monad.IO.Class    (MonadIO, liftIO)

import qualified Data.Aeson                as Aeson
import qualified Data.ByteString.Lazy.UTF8 as B
import qualified Data.Maybe                as Maybe
import qualified Data.String               as String
import qualified Data.Text                 as T

import qualified System.Directory          as Directory
import qualified System.Process            as Process

import           Types                     (TaskwarriorTask)

data Taskwarrior = Taskwarrior
    { task :: [String] -> IO String
    , taskWithStdin :: [String] -> String -> IO String
    , taskGet :: String -> IO String
    , taskExport :: forall m. (MonadError String m, MonadIO m) => [String] -> m [TaskwarriorTask]
    , taskImport :: TaskwarriorTask -> IO String
    }

requireTaskwarrior :: (MonadError String m, MonadIO m) => String -> m Taskwarrior
requireTaskwarrior minVersion = do
    maybeTaskCmd <- liftIO $ Directory.findExecutable "task"
    when (Maybe.isNothing maybeTaskCmd)
         (throwError "Taskwarrior not installed or executable not in PATH.")
    (_, taskwarriorVersion, _) <-
        liftIO $ Process.readProcessWithExitCode "task" ["--version"] ""
    when (taskwarriorVersion < minVersion)
         (throwError $ "Found Taskwarrior " <> taskwarriorVersion
                    <> " installed. Version " <> minVersion
                    <> " or higher required.")
    return $ Taskwarrior
        task'
        taskWithStdin'
        taskGet'
        taskExport'
        taskImport'

task' :: [String] -> IO String
task' cmd = taskWithStdin' cmd ""

taskWithStdin' :: [String] -> String -> IO String
taskWithStdin' cmd stdin =
    (\(_, result, _) -> result)
        <$> Process.readProcessWithExitCode "task" cmd stdin

taskGet' :: String -> IO String
taskGet' str =
    T.unpack . T.strip . T.pack
        <$> task' ["rc.hooks=off", "_get", str]

taskExport' :: (MonadError String m, MonadIO m) => [String] -> m [TaskwarriorTask]
taskExport' filters = do
    exportedTask <- liftIO $ task' (filters ++ ["export"])
    liftEither . Aeson.eitherDecode . String.fromString $ exportedTask

taskImport' :: TaskwarriorTask -> IO String
taskImport' twTask =
    T.unpack . T.strip . T.pack
        <$> taskWithStdin' ["import", "-"] (B.toString $ Aeson.encode twTask)
