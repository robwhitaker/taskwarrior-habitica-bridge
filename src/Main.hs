{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}


module Main where

import           Control.Monad             (void, when)
import           Control.Monad.Except      (ExceptT, liftEither, runExceptT,
                                            throwError)
import           Control.Monad.IO.Class    (liftIO)
import qualified Control.Newtype.Generics  as NT

import           Data.Aeson                (eitherDecode, encode)
import           Data.Aeson.Types          (emptyObject)
import qualified Data.ByteString.Lazy.UTF8 as B
import qualified Data.HashMap.Strict       as HM
import           Data.Maybe                (fromMaybe, isNothing)
import           Data.String               (fromString)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.UUID                 as UUID

import qualified Network.HTTP.Req          as Req

import           System.Environment        (getArgs)
import           System.Exit               (exitFailure)
import qualified System.Process            as Process

import           TaskUtils
import           Types
import           Web

-- Taskwarrior command without STDIN
twCmd :: [String] -> (String -> a) -> IO a
twCmd cmd = twCmd' cmd ""

-- Taskwarrior command with STDIN
twCmd' :: [String] -> String -> (String -> a) -> IO a
twCmd' cmd stdin f =
    (\(_, result, _) -> f result) <$> Process.readProcessWithExitCode "task" cmd stdin

twGet :: String -> IO Text
twGet str = twCmd ["_get", str] (T.strip . T.pack)

twExport :: [String] -> IO [TaskwarriorTask]
twExport filters =
    twCmd (filters ++ ["export"]) (either error id . eitherDecode . fromString)

twImport :: TaskwarriorTask -> IO Text
twImport task = twCmd' ["import", "-"] (B.toString $ encode task) (T.strip . T.pack)

getHabiticaHeaders :: IO HabiticaHeaders
getHabiticaHeaders = do
    -- TODO: this assume (a) task is installed and (b) there will be no error
    habiticaId <- twGet "rc.habitica.user_id"
    habiticaApiKey <- twGet "rc.habitica.api_key"
    case habiticaHeaders habiticaId habiticaApiKey of
        Nothing      -> error "you goofed bruh"
        Just headers -> return headers

-- Return a list of taskwarrior tasks:
--     First item in the tuple is a list of pending tasks without habitica uuids
--     Second item in the tuple is a list of all tasks with habitica uuids
getTaskwarriorTasks :: IO ([TaskwarriorTask], [TaskwarriorTask])
getTaskwarriorTasks = do
    pending <- twExport ["status:pending", "habitica_uuid.none:"]
    habiticaUuid <- twExport ["habitica_uuid.any:"]
    return (pending, habiticaUuid)

getHabiticaTasks :: HabiticaHeaders -> IO [HabiticaTask]
getHabiticaTasks headers = do
    completed <- runHabiticaReq (habiticaGetTasks headers (Just "completedTodos"))
        >>= habiticaResponseHandle
    -- TODO: This could be made more efficient by just doing one request and filtering
    -- out the tasks we don't want, but the decoders would have to be updated to handle
    -- different shapes.
    todos <- runHabiticaReq (habiticaGetTasks headers (Just "todos"))
        >>= habiticaResponseHandle
    dailies <- runHabiticaReq (habiticaGetTasks headers (Just "dailys"))
        >>= habiticaResponseHandle
    -- We don't care about rewards or habits
    return $ todos <> dailies <> completed

pushTaskwarriorTask
    :: HabiticaHeaders
    -> TaskwarriorTask
    -> ExceptT String IO (TaskwarriorTask, Maybe HabiticaUserStats, Maybe ItemDrop)
pushTaskwarriorTask headers taskwarriorTask@(TaskwarriorTask twTask) = do
    let htask@(HabiticaTask task) = toHabiticaTask taskwarriorTask
    let req = habiticaCreateOrUpdateRequest headers htask
    res <- liftIO $ runHabiticaReq req
        >>= betterResponseHandle
    (ResponseData
        (HabiticaTask Task{taskHabiticaId})
        maybeNewStats
        maybeItemDrop) <- liftEither res
    return
        (TaskwarriorTask twTask {taskHabiticaId = taskHabiticaId}
        , maybeNewStats
        , maybeItemDrop
        )

-- TODO: rename this to something better
modifyTaskwarriorTask ::
       HabiticaHeaders
    -> TaskwarriorTask
    -> TaskwarriorTask
    -> ExceptT String IO (TaskwarriorTask, Maybe HabiticaUserStats, Maybe ItemDrop)
modifyTaskwarriorTask headers (TaskwarriorTask oldTask) twTask@(TaskwarriorTask newTask) =
    -- Since the equality check only checks fields shared between Taskwarrior and Habitica,
    -- changing Taskwarrior-only fields will simply return the modified task to Taskwarrior
    -- without an unnecessary request to Habitica's API.
    if oldTask == newTask then
        return (twTask, Nothing, Nothing)
    else case (taskStatus oldTask, taskStatus newTask) of
        -- The task remains deleted and doesn't exist on Habitica,
        -- so return the task unchanged
        (Deleted, Deleted) -> return (twTask, Nothing, Nothing)

        -- A previously deleted task is being brought back on the Taskwarrior
        -- side, so a new Habitica task must be created since we can't recover
        -- a deleted Habitica task
        (Deleted, newStatus) -> do
            -- Ignore the stats and drops as we are creating a new task so they are irrelevant
            (newTwTask@(TaskwarriorTask Task{taskHabiticaId}), _, _) <- pushTaskwarriorTask headers twTask
            hId <- requireHabiticaId
                "Attempt to push task to Habitica resulted in a local task with no UUID."
                taskHabiticaId
            if newStatus == Completed then do
                res <- liftIO $ runHabiticaReq (habiticaScoreTask headers hId Up)
                    >>= betterResponseHandle
                (ResponseData _ maybeNewStats maybeItemDrop) <- liftEither res
                return (newTwTask, maybeNewStats, maybeItemDrop)
            else
                return (newTwTask, Nothing, Nothing)

        -- When going from Completed to Pending, "uncheck" the task
        -- on Habitica.
        (Completed, Pending) -> do
            hId <- getId
            res <- liftIO $ runHabiticaReq (habiticaScoreTask headers hId Down)
                >>= betterResponseHandle
            (ResponseData _ maybeNewStats maybeItemDrop) <- liftEither res
            return (twTask, maybeNewStats, maybeItemDrop)

        -- When going from Pending to Completed, "check" the task
        -- on Habitica.
        (Pending, Completed) -> do
            hId <- getId
            res <- liftIO $ runHabiticaReq (habiticaScoreTask headers hId Up)
                >>= betterResponseHandle
            (ResponseData _ maybeNewStats maybeItemDrop) <- liftEither res
            return (twTask, maybeNewStats, maybeItemDrop)

        -- If the task was deleted (and wasn't already deleted, checked for above)
        -- delete the task on Habitica.
        (_, Deleted) -> do
            hId <- getId
            liftIO $ runHabiticaReq (habiticaDeleteTask headers hId)
            -- Unset the Habitica ID since the task no longer exists
            return
                (TaskwarriorTask newTask{taskHabiticaId = Nothing}
                , Nothing
                , Nothing
                )

        -- The task status didn't change so just update the details.
        (_, _) -> do
            (newTwTask, _, _) <- pushTaskwarriorTask headers twTask
            return (newTwTask, Nothing, Nothing)
  where
    requireHabiticaId :: Monad m => String -> Maybe UUID -> ExceptT String m UUID
    requireHabiticaId errMsg = maybe (throwError errMsg) return

    getId :: Monad m => ExceptT String m UUID
    getId = requireHabiticaId
        "Task has no Habitica ID and cannot be updated."
        (taskHabiticaId newTask)

betterResponseHandle :: HabiticaResponse a -> IO (Either String (ResponseData a))
betterResponseHandle res =
    case res of
        HttpException e ->
            return $ Left ("Something went wrong with the network request: " <> show e)
        ErrorResponse errText errMessage ->
            return $ Left (T.unpack $ errText <> ": " <> errMessage)
        ParseError errText ->
            return $
            Left
                ("Something went wrong while parsing the response from Habitica: " <>
                 errText)
        DataResponse dataRes -> return $ Right dataRes

habiticaResponseHandle :: HabiticaResponse a -> IO a
habiticaResponseHandle (HttpException e) =
    error $ "Something went wrong with the network request: " <> show e
habiticaResponseHandle (ErrorResponse errText errMessage) =
    error $ T.unpack $ errText <> ": " <> errMessage
habiticaResponseHandle (ParseError errText) =
    error $ "Something went wrong while parsing the response from Habitica: " <> errText
habiticaResponseHandle (DataResponse response) = return (resBody response)

addToHabitica :: HabiticaHeaders -> TaskwarriorTask -> IO ()
addToHabitica headers twTask@(TaskwarriorTask task) = do
    let habiticaTask = toHabiticaTask twTask
    -- Create the taskwarrior task on Habitica and retrieve the id of the returned task
    (HabiticaTask Task{taskHabiticaId}) <-
        runHabiticaReq (habiticaCreateOrUpdateRequest headers habiticaTask) >>=
        habiticaResponseHandle
    -- Update the task in taskwarrior with the new id
    twImport (TaskwarriorTask $ task {taskHabiticaId = taskHabiticaId})
    return ()

updateHabitica :: HabiticaHeaders -> HabiticaTask -> HasStatusChange -> IO ()
updateHabitica headers hTask@(HabiticaTask task) hasStatusChange = do
    -- Update the task on Habitica
    runHabiticaReq (habiticaCreateOrUpdateRequest headers hTask) >>=
        habiticaResponseHandle
    -- If the status changed, we need to "score" the task to change it on Habitica
    when hasStatusChange $ do
            let taskId =
                    fromMaybe
                        (error "Trying to update a Habitica task with no ID.")
                        (taskHabiticaId task)
            case taskStatus task of
                Pending ->
                    void $ runHabiticaReq (habiticaScoreTask headers taskId Down)
                Completed ->
                    void $ runHabiticaReq (habiticaScoreTask headers taskId Up)
                Deleted ->
                    void $ runHabiticaReq (habiticaDeleteTask headers taskId)

main :: IO ()
main = do
    headers <- getHabiticaHeaders
    args <- getArgs
    case args of
        ("sync":_) -> sync headers
        ("add":_) -> addHook headers
        ("modify":_) -> modifyHook headers
        _ -> do
            putStrLn "You must provide a valid action: sync, add, modify"
            exitFailure

runAndFailOnError :: ExceptT String IO a -> IO a
runAndFailOnError m = do
    result <- runExceptT m
    case result of
        Left err -> do
            putStrLn $ "ERROR: " <> err
            exitFailure
        Right val -> return val

fetchStats :: HabiticaHeaders -> ExceptT String IO HabiticaUserStats
fetchStats headers = do
    userStatsEither <- liftIO $ runHabiticaReq (habiticaGetUserStats headers)
        >>= betterResponseHandle
    (ResponseData _ maybeStats _) <- liftEither userStatsEither
    maybe
        (throwError "Unable to fetch stats for the user.")
        return
        maybeStats

addHook :: HabiticaHeaders -> IO ()
addHook headers =
    runAndFailOnError $ do
        taskJson <- liftIO getLine
        task <- liftEither $ eitherDecode (fromString taskJson)
        oldUserStats <- fetchStats headers
        -- Stats shouldn't change on adding a task, so ignore them
        (newTask, _, _) <- pushTaskwarriorTask headers task
        liftIO $ putStrLn $ B.toString $ encode newTask

modifyHook :: HabiticaHeaders -> IO ()
modifyHook headers =
    runAndFailOnError $ do
        (oldTaskJson, newTaskJson) <- liftIO $ (,) <$> getLine <*> getLine
        oldTask <- liftEither $ eitherDecode (fromString oldTaskJson)
        newTask <- liftEither $ eitherDecode (fromString newTaskJson)
        oldUserStats <- fetchStats headers
        (newerTask, maybeStats, maybeDrop) <- modifyTaskwarriorTask headers oldTask newTask

        case maybeStats of
            Nothing -> return ()
            Just newStats -> liftIO $ mapM_ putStrLn (getUserStatDiffs oldUserStats newStats)

        case maybeDrop of
            Nothing -> return ()
            Just (ItemDrop itemDropMsg) -> liftIO $ putStrLn "" >> putStrLn (T.unpack itemDropMsg)

        liftIO $ putStrLn $ B.toString $ encode newerTask

sync :: HabiticaHeaders -> IO ()
sync headers = do
    habiticaTasks <- getHabiticaTasks headers
    (twOnlyTasks, twHabiticaSyncedTasks) <- getTaskwarriorTasks
    let hTasks =
            foldr
                (\hTask@(HabiticaTask task) taskMap ->
                     HM.insert (NT.unpack <$> taskHabiticaId task) hTask taskMap)
                HM.empty
                habiticaTasks
    let twTasks =
            foldr
                (\twTask@(TaskwarriorTask task) taskMap ->
                     HM.insert (NT.unpack <$> taskHabiticaId task) twTask taskMap)
                HM.empty
                twHabiticaSyncedTasks
    -- Tasks that exist in Habitica that Taskwarrior doesn't know about yet
    let habiticaOnlyTasks = HM.difference hTasks twTasks
    -- Tasks that have previously synced between Taskwarrior and Habitica
    -- but have since been deleted from Habitica
    let deletedFromHabitica = HM.difference twTasks hTasks
    let taskUpdates =
            HM.mapMaybe id $
            HM.intersectionWith
                (\h@(HabiticaTask hTask) t@(TaskwarriorTask twTask) ->
                     if hTask == twTask
                         then Nothing
                         else if taskModified hTask > taskModified twTask
                    -- Habitica was updated more recently, so update Taskwarrior
                                  then Just $
                                       UpdateTaskwarrior $ updateTaskwarriorTask h t
                    -- Taskwarrior was updated more recently, so update Habitica
                                  else Just $
                                       UpdateHabitica
                                           (updateHabiticaTask t h)
                                           (taskStatus hTask /= taskStatus twTask))
                hTasks
                twTasks
    let changes =
            map Update (HM.elems taskUpdates) <>
            map AddToHabitica twOnlyTasks <>
            map AddToTaskwarrior (HM.elems habiticaOnlyTasks) <>
            map DeleteFromTaskwarrior (HM.elems deletedFromHabitica)
    mapM_
        (\case
            AddToTaskwarrior habiticaTask ->
                void $ twImport (toTaskwarriorTask habiticaTask)
            DeleteFromTaskwarrior taskwarriorTask@(TaskwarriorTask task) ->
                void $ twImport (TaskwarriorTask task{taskStatus = Deleted})
            AddToHabitica taskwarriorTask -> addToHabitica headers taskwarriorTask
            Update (UpdateTaskwarrior taskwarriorTask) ->
                void $ twImport taskwarriorTask
            Update (UpdateHabitica habiticaTask hasStatusChange) ->
                updateHabitica headers habiticaTask hasStatusChange)
        changes


getUserStatDiffs :: HabiticaUserStats -> HabiticaUserStats -> [String]
getUserStatDiffs old new
    | lvlDiff /= 0 =
        [ if lvlDiff > 0 then
            "You leveled up! You are now level " <> show (statsLvl new) <> "!"
          else
            "You lost a level. You are now level " <> show (statsLvl new) <> "."
        , mkDiffText "hp" statsHp
        , mkDiffText "mp" statsMp
        , mkDiffText "gold" statsGp
        ]
    | otherwise =
        [ mkDiffText "hp" statsHp
        , mkDiffText "mp" statsMp
        , mkDiffText "gold" statsGp
        , mkDiffText "exp" statsExp
        ]
  where
    lvlDiff = statsLvl new - statsLvl old

    mkDiffText field getter
        | diff == 0 = "You have " <> prettyShowField new <> " " <> field <> "."
        | otherwise =
            let (dir, punc) = if diff > 0 then ("gained", "!") else ("lost", ".")
                diffVal = prettyShow (abs diff)
            in mconcat
                [ "You ", dir, " ", diffVal, " ", field, punc
                , " (current ", field, ": ", prettyShowField new, ")"
                ]
      where diff = getter new - getter old
            rounded n = fromIntegral (round (n * 100)) / 100
            prettyShow = show . rounded
            prettyShowField = prettyShow . getter
