{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}


module Main where

import           Control.Exception         (tryJust)
import           Control.Monad             (foldM_, forM_, guard, void, when)
import           Control.Monad.Except      (ExceptT, liftEither, runExceptT,
                                            throwError)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Reader      (ReaderT, ask, runReaderT)
import           Control.Monad.Trans.Class (lift)

import           Control.Newtype.Generics  (Newtype, O)
import qualified Control.Newtype.Generics  as NT

import           Data.Aeson                (decodeFileStrict', eitherDecode,
                                            encode, encodeFile)
import qualified Data.ByteString.Lazy.UTF8 as B
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HM
import           Data.Maybe                (catMaybes, fromMaybe)
import qualified Data.Set                  as Set
import           Data.String               (fromString)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.UUID                 as UUID

import           System.Directory          (getHomeDirectory, removeFile)
import           System.Environment        (getArgs, lookupEnv, setEnv,
                                            unsetEnv)
import           System.Exit               (ExitCode (..), exitFailure)
import           System.FilePath.Posix     ((</>))
import           System.IO.Error           (isDoesNotExistError)
import qualified System.Process            as Process

import           TaskUtils
import           Types
import           Web

-- Minimum Taskwarrior version
-- (for proper "import" functionality)
minTaskwarriorVersion :: String
minTaskwarriorVersion = "2.5.0"

-- Taskwarrior command without STDIN
twCmd :: [String] -> (String -> a) -> IO a
twCmd cmd = twCmd' cmd ""

-- Taskwarrior command with STDIN
twCmd' :: [String] -> String -> (String -> a) -> IO a
twCmd' cmd stdin f =
    (\(_, result, _) -> f result)
        <$> Process.readProcessWithExitCode "task" cmd stdin

twGet :: String -> IO Text
twGet str = twCmd ["rc.hooks=off", "_get", str] (T.strip . T.pack)

twExport :: [String] -> IO [TaskwarriorTask]
twExport filters =
    twCmd (filters ++ ["export"]) (either error id . eitherDecode . fromString)

twImport :: TaskwarriorTask -> IO Text
twImport task = twCmd' ["import", "-"] (B.toString $ encode task) (T.strip . T.pack)

twGetVersion :: IO (Maybe String)
twGetVersion = do
    (exitCode, _, _) <- Process.readProcessWithExitCode "which" ["task"] ""
    case exitCode of
        ExitFailure _ -> return Nothing
        ExitSuccess   -> (\(_, result, _) -> Just $ T.unpack $ T.strip $ T.pack result) <$>
            Process.readProcessWithExitCode "task" ["--version"] ""

getHabiticaHeaders :: ExceptT String IO HabiticaHeaders
getHabiticaHeaders = do
    habiticaId <- liftIO $ twGet "rc.habitica.user_id"
    habiticaApiKey <- liftIO $ twGet "rc.habitica.api_key"
    case habiticaHeaders habiticaId habiticaApiKey of
        Nothing -> throwError "Missing or malformed Habitica credentials in taskrc."
        Just headers -> return headers

-- Return a list of taskwarrior tasks:
--     First item in the tuple is a list of pending tasks without habitica uuids
--     Second item in the tuple is a list of all tasks with habitica uuids
getTaskwarriorTasks :: IO ([TaskwarriorTask], [TaskwarriorTask])
getTaskwarriorTasks = do
    pending <- twExport ["status:pending", "habitica_uuid.none:"]
    habiticaUuid <- twExport ["habitica_uuid.any:"]
    return (pending, habiticaUuid)

getHabiticaTasks :: HabiticaHeaders -> ExceptT String IO [HabiticaTask]
getHabiticaTasks headers = do
    completed <- liftIO (runHabiticaReq (habiticaGetTasks headers (Just "_allCompletedTodos")))
        >>= betterResponseHandle
    -- TODO: This could be made more efficient by just doing one request and filtering
    -- out the tasks we don't want, but the decoders would have to be updated to handle
    -- different shapes.
    todos <- liftIO (runHabiticaReq (habiticaGetTasks headers (Just "todos")))
        >>= betterResponseHandle
    dailies <- liftIO (runHabiticaReq (habiticaGetTasks headers (Just "dailys")))
        >>= betterResponseHandle
    -- We don't care about rewards or habits
    return $ mconcat $ fmap resBody [todos, dailies, completed]

pushTaskwarriorTask :: HabiticaHeaders -> TaskwarriorTask -> ExceptT String IO TaskwarriorTask
pushTaskwarriorTask headers taskwarriorTask@(TaskwarriorTask twTask) = do
    newHabiticaId <-
        case toHabiticaTask taskwarriorTask of
            Nothing -> return Nothing
            Just htask -> do
                let req = habiticaCreateOrUpdateRequest headers htask
                (ResponseData (HabiticaTask Task{taskHabiticaId}) _ _) <-
                    liftIO (runHabiticaReq req) >>= betterResponseHandle
                return taskHabiticaId
    return $ TaskwarriorTask twTask {taskHabiticaId = newHabiticaId}

-- TODO: rename this to something better
modifyTaskwarriorTask ::
       HabiticaHeaders
    -> TaskwarriorTask
    -> TaskwarriorTask
    -> ExceptT String IO (TaskwarriorTask, Maybe HabiticaUserStats, Maybe ItemDrop)
modifyTaskwarriorTask headers (TaskwarriorTask oldTask) twTask@(TaskwarriorTask newTask) =
    case (taskStatus oldTask, taskStatus newTask) of
        (oldStatus, newStatus)
            -- The task doesn't and shouldn't exist on Habitica,
            -- so return the task unchanged
            | notOnHabitica oldStatus && notOnHabitica newStatus ->
                return (twTask, Nothing, Nothing)

            -- The task previously existed on Habitica but should not anymore,
            -- so delete it from Habitica, unset the Habitica ID in Taskwarrior,
            -- and return the Taskwarrior task
            | onHabitica oldStatus && notOnHabitica newStatus -> do
                case taskHabiticaId oldTask of
                    Nothing -> return ()
                    Just hId ->
                        -- We don't need the response, but we want to validate that the
                        -- request went through okay
                        void $ liftIO (runHabiticaReq (habiticaDeleteTask headers hId))
                            >>= betterResponseHandle
                return
                    ( TaskwarriorTask newTask { taskHabiticaId = Nothing }
                    , Nothing
                    , Nothing
                    )

            -- A task that previously did not exist on Habitica now has a status
            -- that should exist on Habitica, so create a new task on Habitica
            -- and return the Taskwarrior task with the new Habitica ID
            | notOnHabitica oldStatus && onHabitica newStatus -> do
                newTwTask@(TaskwarriorTask Task{taskHabiticaId}) <- pushTaskwarriorTask headers twTask
                hId <- requireHabiticaId
                    "Attempt to push task to Habitica resulted in a local task with no UUID."
                    taskHabiticaId
                if newStatus == TWCompleted then do
                    (ResponseData _ maybeNewStats maybeItemDrop) <-
                        liftIO (runHabiticaReq (habiticaScoreTask headers hId Up))
                            >>= betterResponseHandle
                    return (newTwTask, maybeNewStats, maybeItemDrop)
                else
                    return (newTwTask, Nothing, Nothing)

            -- When going from Completed to Pending or Waiting, "uncheck" the task
            -- on Habitica
            | completed oldStatus && pending newStatus ->
                scoreOnHabitica Down

            -- When going from Pending or Waiting to Completed, "check" the task
            -- on Habitica
            | pending oldStatus && completed newStatus ->
                scoreOnHabitica Up

            -- The task status didn't change so just update the details
            | otherwise -> do
                newTwTask <- pushTaskwarriorTask headers twTask
                return (newTwTask, Nothing, Nothing)
  where
    onHabitica = (`elem` [TWPending, TWWaiting, TWCompleted])
    notOnHabitica = (`elem` [TWRecurring, TWDeleted])
    completed = (== TWCompleted)
    pending = (`elem` [TWPending, TWWaiting])

    requireHabiticaId :: Monad m => String -> Maybe UUID -> ExceptT String m UUID
    requireHabiticaId errMsg = maybe (throwError errMsg) return

    getId :: Monad m => ExceptT String m UUID
    getId = requireHabiticaId
        "Task has no Habitica ID and cannot be updated."
        (taskHabiticaId newTask)

    scoreOnHabitica dir = do
        hId <- getId
        (ResponseData _ maybeNewStats maybeItemDrop) <-
            liftIO (runHabiticaReq (habiticaScoreTask headers hId dir))
                >>= betterResponseHandle
        return (twTask, maybeNewStats, maybeItemDrop)

betterResponseHandle :: HabiticaResponse a -> ExceptT String IO (ResponseData a)
betterResponseHandle res =
    case res of
        HttpException e ->
            throwError ("Something went wrong with the network request: " <> show e)
        ErrorResponse errText errMessage ->
            throwError (T.unpack $ errText <> ": " <> errMessage)
        ParseError errText ->
            throwError
                ("Something went wrong while parsing the response from Habitica: " <> errText)
        DataResponse dataRes -> return dataRes

envVarName :: String
envVarName = "TASKBITICA_RUNNING"

main :: IO ()
main = runAndFailOnError $ do
    twVersion <- liftIO twGetVersion
    version <- maybe
        (throwError "Taskwarrior not installed or executable not in PATH.")
        return
        twVersion
    when (version < minTaskwarriorVersion) $
        throwError $ "Found Taskwarrior " <> version <> " installed. Version " <> minTaskwarriorVersion <>
                     " or higher required."
    headers <- getHabiticaHeaders
    args <- liftIO getArgs
    case args of
        ("sync":rest) -> do
            let syncArgs = foldl (\acc arg ->
                    case arg of
                        "--verbose" -> acc{syncVerbose = True}
                        _           -> acc
                    ) (SyncArgs False) rest
            -- Set an environment variable so hooks know not to run during sync
            liftIO $ setEnv envVarName "1"
            runReaderT (sync headers) syncArgs
            liftIO $ unsetEnv envVarName
        ("add":_) -> do
            maybeEnvVar <- liftIO $ lookupEnv envVarName
            case maybeEnvVar of
                Nothing -> addHook headers
                Just _  -> liftIO $ getLine >>= putStrLn
        ("modify":_) -> do
            maybeEnvVar <- liftIO $ lookupEnv envVarName
            case maybeEnvVar of
                Nothing -> modifyHook headers
                Just _  -> liftIO $ getLine >> getLine >>= putStrLn
        ("exit":_) -> liftIO $ do
            statCache <- getStatsCacheFilePath
            tryJust (guard . isDoesNotExistError) (removeFile statCache) >>=
                const (return ())
        _ -> throwError "You must provide a valid action: sync, add, modify"

runAndFailOnError :: ExceptT String IO a -> IO a
runAndFailOnError m = do
    result <- runExceptT m
    case result of
        Left err -> do
            putStrLn $ "ERROR: " <> err
            liftIO $ unsetEnv envVarName
            exitFailure
        Right val -> return val

fetchStats :: HabiticaHeaders -> ExceptT String IO HabiticaUserStats
fetchStats headers = do
    (ResponseData _ maybeStats _) <-
        liftIO (runHabiticaReq (habiticaGetUserStats headers))
            >>= betterResponseHandle
    maybe (throwError "Unable to fetch stats for the user.") return maybeStats

addHook :: HabiticaHeaders -> ExceptT String IO ()
addHook headers = do
    taskJson <- liftIO getLine
    task <- liftEither $ eitherDecode (fromString taskJson)
    newTask <- pushTaskwarriorTask headers task
    liftIO $ putStrLn $ B.toString $ encode newTask

modifyHook :: HabiticaHeaders -> ExceptT String IO ()
modifyHook headers = do
    (oldTaskJson, newTaskJson) <- liftIO $ (,) <$> getLine <*> getLine
    oldTask <- liftEither $ eitherDecode (fromString oldTaskJson)
    newTask <- liftEither $ eitherDecode (fromString newTaskJson)
    -- Turn the old and new Taskwarrior tasks into Habitica tasks; if
    -- they are equal, then we don't need to push anything to Habitica.
    if toHabiticaTask oldTask == toHabiticaTask newTask then
        printTask newTask
    else do
        maybeDecodedFile <- liftIO getHabiticaStatsFromCacheFile

        oldUserStats <- case maybeDecodedFile of
            Nothing    -> fetchStats headers
            Just stats -> return stats

        (newerTask, maybeStats, maybeDrop) <- modifyTaskwarriorTask headers oldTask newTask

        case maybeStats of
            Nothing -> return ()
            Just newStats -> liftIO $ do
                dataFile <- getStatsCacheFilePath
                encodeFile dataFile newStats
                mapM_ putStrLn (getUserStatDiffs oldUserStats newStats)

        case maybeDrop of
            Nothing -> return ()
            Just (ItemDrop itemDropMsg) -> liftIO $ putStrLn "" >> putStrLn (T.unpack itemDropMsg)

        printTask newerTask
  where
    printTask = liftIO . putStrLn . B.toString . encode

getHabiticaStatsFromCacheFile :: IO (Maybe HabiticaUserStats)
getHabiticaStatsFromCacheFile = do
    dataFile <- getStatsCacheFilePath
    tryJust (guard . isDoesNotExistError) (decodeFileStrict' dataFile)
        >>= either (const $ return Nothing) return

getStatsCacheFilePath :: IO FilePath
getStatsCacheFilePath =
    (</> "cached_habitica_stats.json") <$> getTaskwarriorDataDir

getTaskwarriorDataDir :: IO FilePath
getTaskwarriorDataDir = do
    dataDir <- T.unpack <$> twGet "rc.data.location"
    expandPath dataDir
  where expandPath :: FilePath -> IO FilePath
        expandPath fp =
            if take 1 fp == "~"
                then do home <- getHomeDirectory
                        return $ home </> dropWhile (=='/') (drop 1 fp)
                else return fp

sync :: HabiticaHeaders -> ReaderT SyncArgs (ExceptT String IO) ()
sync headers = do
    args <- ask
    let verbose = syncVerbose args
    habiticaTasks <- lift $ getHabiticaTasks headers
    (twOnlyTasks, twHabiticaSyncedTasks) <- liftIO getTaskwarriorTasks

    -- The task exists in Taskwarrior but not on Habitica, so we have to push it to Habitica
    -- and set the new Habitica ID in Taskwarrior
    lift $ forM_ twOnlyTasks $ \twTask -> do
        liftIO $ do
            putStrLn $ "Task: " <> T.unpack (taskText (NT.unpack twTask))
            putStrLn "    Status: Created in Taskwarrior."
            putStrLn "    Action: Pushing to Habitica and updating Habitica ID in Taskwarrior."
            putStrLn ""
        newTwTask <- pushTaskwarriorTask headers twTask
        void $ liftIO $ twImport newTwTask

    -- Handle tasks that exist in Habitica
    let hTasks = tasksToHM habiticaTasks
    let twSyncedTasks = tasksToHM twHabiticaSyncedTasks
    let keys = Set.fromList $ HM.keys hTasks <> HM.keys twSyncedTasks
    userStats <- lift (fetchStats headers)
    lift $ foldM_ (\stats key ->
        case (HM.lookup key hTasks, HM.lookup key twSyncedTasks) of
            -- This should never occur, but if it somehow ever does, at least it has a descriptive error
            (Nothing, Nothing) ->
                throwError $ "Impossible state. A key was found with no corresponding value, " <>
                             "despite existing in one of the hashmaps. Key was: " <> show key

            -- The task exists on Habitica but not in Taskwarrior, so we have to import it to Taskwarrior
            (Just habiticaTask, Nothing) -> liftIO $ do
                putStrLn $ "Task: " <> T.unpack (taskText (NT.unpack habiticaTask))
                putStrLn "    Status: Created on Habitica."
                putStrLn "    Action: Importing into Taskwarrior."
                putStrLn ""
                void $ twImport (toTaskwarriorTask habiticaTask)
                return stats

            -- The task does not exist on Habitica, but it was previously synced with Taskwarrior.
            -- This means it used to exist on Habitica and was deleted, so set it to Deleted in Taskwarrior.
            (Nothing, Just (TaskwarriorTask task)) -> liftIO $ do
                putStrLn $ "Task: " <> T.unpack (taskText task)
                putStrLn "    Status: Deleted on Habitica."
                case taskStatus task of
                    TWCompleted -> do
                        putStrLn "    Action: Already completed in Taskwarrior. Leaving status as Completed. Unsetting Habitica ID."
                        void $ liftIO $ twImport (TaskwarriorTask task{taskHabiticaId = Nothing})
                    _ -> do
                        putStrLn "    Action: Setting status to Deleted in Taskwarrior. Unsetting Habitica ID."
                        void $ liftIO $ twImport (TaskwarriorTask
                            task{ taskStatus = TWDeleted
                                , taskHabiticaId = Nothing
                                })
                putStrLn ""
                return stats

            -- The task exists on both sides, so we'll take the most recently modified of the two
            -- and update the other to match
            (Just habiticaTask, Just taskwarriorTask) ->
                if Just habiticaTask == toHabiticaTask taskwarriorTask then do
                    when verbose $
                        liftIO $ do
                            bothSidesLog habiticaTask taskwarriorTask
                            putStrLn "    Action: Tasks are equal. Doing nothing."
                            putStrLn ""
                    return stats
                else do
                    liftIO $ bothSidesLog habiticaTask taskwarriorTask
                    newStats <- updateFromNewest habiticaTask taskwarriorTask stats
                    liftIO $ putStrLn ""
                    return $ fromMaybe stats newStats
        ) userStats keys
  where
    tasksToHM :: (Newtype n, O n ~ Task status) => [n] -> HashMap (Maybe UUID.UUID) n
    tasksToHM =
        foldr (\wrappedTask -> HM.insert
            (NT.unpack <$> taskHabiticaId (NT.unpack wrappedTask)) wrappedTask
        ) HM.empty

    updateFromNewest
        :: HabiticaTask
        -> TaskwarriorTask
        -> HabiticaUserStats
        -> ExceptT String IO (Maybe HabiticaUserStats)
    updateFromNewest habiticaTask@(HabiticaTask hTask) taskwarriorTask@(TaskwarriorTask twTask) currentStats =
        if taskModified hTask > taskModified twTask then do
            -- If the task was modified most recently on Habitica, we want to update
            -- the Taskwarrior task with the Habitica details and import it back
            -- into Taskwarrior
            liftIO $ putStrLn "    Action: Habitica task is most recently modified. Updating in Taskwarrior"
            void $ liftIO $ twImport (updateTaskwarriorTask habiticaTask taskwarriorTask)
            return Nothing
        else do
            -- If the task was modified most recently on Taskwarrior, we can use the
            -- modifyTaskwarriorTask function to update Habitica by converting our Habitica
            -- task into a Taskwarrior task to fit the type signature
            liftIO $ putStrLn "    Action: Taskwarrior task is most recently modified. Updating on Habitica."
            (newTwTask, maybeStats, maybeDrop) <-
                modifyTaskwarriorTask headers (toTaskwarriorTask habiticaTask) taskwarriorTask
            void $ liftIO $ twImport newTwTask

            case maybeStats of
                Nothing -> return ()
                Just newStats -> liftIO $ mapM_ (putStrLn . ("    " <>)) (getUserStatDiffs currentStats newStats)

            case maybeDrop of
                Nothing -> return ()
                Just (ItemDrop itemDropMsg) -> liftIO $ putStrLn ("    " <> T.unpack itemDropMsg)

            return maybeStats

    bothSidesLog :: HabiticaTask -> TaskwarriorTask -> IO ()
    bothSidesLog habiticaTask taskwarriorTask = do
        putStrLn $ "Habitica Task:    " <> T.unpack (taskText (NT.unpack habiticaTask))
        putStrLn $ "Taskwarrior Task: " <> T.unpack (taskText (NT.unpack taskwarriorTask))
        putStrLn "    Status: Exists on both Habitica and Taskwarrior."

getUserStatDiffs :: HabiticaUserStats -> HabiticaUserStats -> [String]
getUserStatDiffs old new
    | lvlDiff /= 0 = catMaybes
        [ if lvlDiff > 0 then
            Just $ "You leveled up! You are now level " <> show (statsLvl new) <> "!"
          else
            Just $ "You lost a level. You are now level " <> show (statsLvl new) <> "."
        , mkDiffText "hp" statsHp
        , mkDiffText "mp" statsMp
        , mkDiffText "gold" statsGp
        ]
    | otherwise = catMaybes
        [ mkDiffText "mp" statsMp
        , mkDiffText "gold" statsGp
        , mkDiffText "exp" statsExp
        ]
  where
    lvlDiff = statsLvl new - statsLvl old

    mkDiffText field getter
        | diff == 0 = Nothing
        | otherwise =
            let (dir, punc) = if diff > 0 then ("gained", "!") else ("lost", ".")
                diffVal = prettyShow (abs diff)
            in Just $ mconcat
                [ "You ", dir, " ", diffVal, " ", field, punc
                , " (current ", field, ": ", prettyShowField new, ")"
                ]
      where diff = getter new - getter old
            rounded n = fromIntegral (round (n * 100) :: Int) / 100
            prettyShow = show . rounded
            prettyShowField = prettyShow . getter
