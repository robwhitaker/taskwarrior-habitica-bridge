{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Control.Exception         (IOException, catch, tryJust)
import           Control.Monad             (foldM_, forM_, guard, void, when)
import           Control.Monad.Except      (throwError)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Reader      (asks)

import           Control.Newtype.Generics  (Newtype, O)
import qualified Control.Newtype.Generics  as NT

import           Data.Aeson                ((.:))
import qualified Data.Aeson                as Aeson
import qualified Data.Aeson.KeyMap         as KM
import qualified Data.Aeson.Types          as Aeson
import qualified Data.ByteString.Lazy.UTF8 as B
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HM
import qualified Data.List                 as List
import qualified Data.Maybe                as Maybe
import qualified Data.Set                  as Set
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import qualified Data.Time.Clock           as Time
import qualified Data.UUID                 as UUID

import qualified System.Directory          as Dir
import qualified System.Exit               as Exit
import           System.FilePath           ((<.>), (</>))
import qualified System.IO.Error           as IOError

import           App                       (App, AppError, AppReader, Args (..),
                                            Cmd (..), Env (..))
import qualified App
import           TaskUtils
import           Taskwarrior               (Taskwarrior (..))
import           Types
import           Web

-- Running the program

main :: IO ()
main = App.runProgram handleError $ \case
    Add    ->
        switchIfEnvVarSet
            addHook
            (liftIO (getLine >>= putStrLn))

    Modify ->
        switchIfEnvVarSet
            modifyHook
            (liftIO (getLine >> getLine >>= putStrLn))

    Exit   ->
        exitHook

    Sync   ->
        runWithEnvVar sync

  where
    runWithEnvVar :: App () -> App ()
    runWithEnvVar app = do
        App.setEnvVar
        app

    switchIfEnvVarSet :: App () -> App () -> App ()
    switchIfEnvVarSet appNormal appIfEnvVarSet = do
        maybeEnvVar <- App.getEnvVar
        case maybeEnvVar of
            Nothing -> appNormal
            Just _  -> appIfEnvVarSet

handleError :: Error -> IO ()
handleError err = do
    putStrLn $ "Error: " <> err
    Exit.exitFailure

-- Helper functions

getHabiticaStatsFromCacheFile :: (AppReader m, MonadIO m) => m (Maybe HabiticaUserStatsCache)
getHabiticaStatsFromCacheFile = do
    dataFile <- asks envHabiticaUserStatsCache
    liftIO $ tryJust (guard . IOError.isDoesNotExistError) (Aeson.decodeFileStrict' dataFile)
        >>= either (const $ return Nothing) return

{- Return a list of taskwarrior tasks:
--     First item in the tuple is a list of pending tasks without habitica uuids
--     Second item in the tuple is a list of all tasks with habitica uuids
-}
getTaskwarriorTasks :: App ([TaskwarriorTask], [TaskwarriorTask])
getTaskwarriorTasks = do
    Taskwarrior{taskExport} <- asks envTaskwarrior
    pending <- taskExport ["status:pending", "habitica_uuid.none:"]
    habiticaUuid <- taskExport ["habitica_uuid.any:"]
    return (pending, habiticaUuid)

getHabiticaTasks :: App [HabiticaTask]
getHabiticaTasks = do
    headers <- asks envHabiticaHeaders
    completed <- liftIO (runHabiticaReq (habiticaGetTasks headers (Just "_allCompletedTodos")))
        >>= responseHandle
    todos <- liftIO (runHabiticaReq (habiticaGetTasks headers (Just "todos")))
        >>= responseHandle
    dailies <- liftIO (runHabiticaReq (habiticaGetTasks headers (Just "dailys")))
        >>= responseHandle
    -- We don't care about rewards or habits
    return $ mconcat $ fmap resBody [todos, dailies, completed]

fetchStats :: App HabiticaUserStats
fetchStats = do
    headers <- asks envHabiticaHeaders
    (ResponseData _ maybeStats _) <-
        liftIO (runHabiticaReq (habiticaGetUserStats headers))
            >>= responseHandle
    maybe (throwError "Unable to fetch stats for the user.") return maybeStats

getUserStatDiffs :: HabiticaUserStatsCache -> [String]
getUserStatDiffs statsCache
    | old == new =
        drops
    | otherwise =
        mconcat
            (Maybe.catMaybes
            $ List.intersperse (Just " | ")
            $ filter Maybe.isJust
                [ mkDiffText "HP" statsHp statsMaxHp
                , mkDiffText "MP" statsMp statsMaxMp
                , if lvlDiff == 0
                    then mkDiffText "Exp" statsExp statsNextLvlExp
                    else Nothing
                , mkDiffText "Gold" statsGp (const Nothing)
                , lvlChangeText
                ])
        : drops
  where
    old = cacheOld statsCache
    new = Maybe.fromMaybe (cacheOld statsCache) (cacheCurrent statsCache)
    drops = List.reverse (T.unpack <$> cacheDrops statsCache)
    lvlDiff = statsLvl new - statsLvl old

    lvlChangeText
        | lvlDiff > 0 =
            Just $ mconcat
                [ "LEVEL UP! ("
                , show (statsLvl old)
                , "->"
                , show (statsLvl new)
                , ")"
                ]
        | lvlDiff < 0 =
            Just $ mconcat
                [ "LEVEL LOST! ("
                , show (statsLvl old)
                , "->"
                , show (statsLvl new)
                , ")"
                ]
        | otherwise =
            Nothing

    mkDiffText field getter maxGetter
        | diff == 0 = Nothing
        | otherwise =
            let dir = if diff > 0 then "+" else "-"
                diffVal = prettyShow (abs diff)
            in Just $ mconcat
                ([ field
                , ":"
                , dir
                , diffVal
                , "("
                , prettyShowField new
                ] <>
                (case maxGetter old of
                    Nothing -> []
                    Just val ->
                        [ "/"
                        , show val
                        ]) <>
                [ ")" ])
      where diff = getter new - getter old
            prettyShow n =
                if n < 1 then
                    show $ fromIntegral (round (n * 100) :: Int) / 100
                else
                    show (round n)
            prettyShowField = prettyShow . getter

-- Updating tasks on Habitica from Taskwarrior

type StatResponse a = (a, Maybe HabiticaUserStats, Maybe ItemDrop)

responseHandle :: AppError m => HabiticaResponse a -> m (ResponseData a)
responseHandle res =
    case res of
        HttpException e ->
            throwError ("Something went wrong with the network request: " <> show e)
        ErrorResponse errText errMessage ->
            throwError (T.unpack $ errText <> ": " <> errMessage)
        ParseError errText ->
            throwError
                ("Something went wrong while parsing the response from Habitica: " <> errText)
        DataResponse dataRes -> return dataRes

pushTaskwarriorTask' :: TaskwarriorTask -> App (Maybe HabiticaTask, TaskwarriorTask)
pushTaskwarriorTask' taskwarriorTask@(TaskwarriorTask twTask) = do
    headers <- asks envHabiticaHeaders
    maybeNewHabiticaTask <-
        case toHabiticaTask taskwarriorTask of
            Nothing -> return Nothing
            Just htask@(HabiticaTask Task{taskStatus}) ->
                -- TODO: Maybe the HDeleted constructor should be removed
                --       as, like TWRecurring, it cannot be represented on
                --       Habitica. For now, we'll just check if the status
                --       is deleted and not push the task if it is.
                if taskStatus == HDeleted
                    then return Nothing
                    else do
                        let req = habiticaCreateOrUpdateRequest headers htask
                        (ResponseData habiticaTask _ _) <-
                            liftIO (runHabiticaReq req) >>= responseHandle
                        return (Just habiticaTask)
    let habiticaId = maybeNewHabiticaTask >>= taskHabiticaId . NT.unpack
    return
        ( maybeNewHabiticaTask
        , TaskwarriorTask twTask {taskHabiticaId = habiticaId}
        )

pushTaskwarriorTask :: TaskwarriorTask -> App TaskwarriorTask
pushTaskwarriorTask = fmap snd . pushTaskwarriorTask'

pushTaskAndCompleteIfNeeded :: TaskwarriorTask -> App (StatResponse TaskwarriorTask)
pushTaskAndCompleteIfNeeded twTask@(TaskwarriorTask task) = do
    headers <- asks envHabiticaHeaders
    (maybeHabiticaTask, newTaskwarriorTask@(TaskwarriorTask newTwTask)) <-
        pushTaskwarriorTask' twTask
    let maybeHId = taskHabiticaId newTwTask
    case (maybeHabiticaTask, maybeHId) of
        (Just (HabiticaTask hTask), Just hId) ->
            if taskStatus hTask == HPending && taskStatus task == TWCompleted
                then do
                    (ResponseData _ maybeNewStats maybeItemDrop) <-
                        liftIO (runHabiticaReq (habiticaScoreTask headers hId Up))
                            >>= responseHandle
                    return (newTaskwarriorTask, maybeNewStats, maybeItemDrop)
                else return (newTaskwarriorTask, Nothing, Nothing)
        _ -> return (twTask, Nothing, Nothing)

modifyTaskwarriorTask
    :: TaskwarriorTask
    -> TaskwarriorTask
    -> App (StatResponse TaskwarriorTask)
modifyTaskwarriorTask (TaskwarriorTask oldTask) twTask@(TaskwarriorTask newTask) = do
    headers <- asks envHabiticaHeaders
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
                            >>= responseHandle
                return
                    ( TaskwarriorTask newTask { taskHabiticaId = Nothing }
                    , Nothing
                    , Nothing
                    )

            -- A task that previously did not exist on Habitica now has a status
            -- that should exist on Habitica, so create a new task on Habitica
            -- and return the Taskwarrior task with the new Habitica ID
            | notOnHabitica oldStatus && onHabitica newStatus ->
                pushTaskAndCompleteIfNeeded twTask

            -- When going from Completed to Pending or Waiting, update the
            -- task details if they've changed and then "uncheck" the task
            -- on Habitica
            | completed oldStatus && pending newStatus ->
                updateTaskDetails >>= scoreOnHabitica Down

            -- When going from Pending or Waiting to Completed, update the
            -- task details if they've changed and then "check" the task
            -- on Habitica
            | pending oldStatus && completed newStatus ->
                updateTaskDetails >>= scoreOnHabitica Up

            -- The task status didn't change so just update the details
            | otherwise -> do
                newTwTask <- updateTaskDetails
                return (newTwTask, Nothing, Nothing)
  where
    onHabitica = (`elem` [TWPending, TWWaiting, TWCompleted])
    notOnHabitica = (`elem` [TWRecurring, TWDeleted])
    completed = (== TWCompleted)
    pending = (`elem` [TWPending, TWWaiting])

    requireHabiticaId :: AppError m => Error -> Maybe UUID -> m UUID
    requireHabiticaId errMsg = maybe (throwError errMsg) return

    getId :: AppError m => TaskwarriorTask -> m UUID
    getId (TaskwarriorTask task) = requireHabiticaId
        "Task has no Habitica ID and cannot be updated."
        (taskHabiticaId task)

    updateTaskDetails :: App TaskwarriorTask
    updateTaskDetails =
        -- If details of the task, other than the status, have
        -- changed, update them on Habitica. Otherwise return
        -- the task unchanged.
        if oldTask /= newTask {taskStatus = taskStatus oldTask}
            then pushTaskwarriorTask twTask
            else return twTask

    scoreOnHabitica dir task = do
        headers <- asks envHabiticaHeaders
        hId <- getId task
        (ResponseData _ maybeNewStats maybeItemDrop) <-
            liftIO (runHabiticaReq (habiticaScoreTask headers hId dir))
                >>= responseHandle
        return (task, maybeNewStats, maybeItemDrop)

-- Cli commands

addHook :: App ()
addHook = do
    taskJson <- liftIO getLine
    task <- App.decodeTaskwarriorJSON taskJson

    -- Only fetch the stats if the task being added is already completed.
    -- Otherwise, it's a waste of a request.
    maybeStatsCache <-
        if taskStatus (NT.unpack task) == TWCompleted
            then fmap Just $
                HabiticaUserStatsCache
                    <$> fetchStats
                    <*> return Nothing
                    <*> return []
            else return Nothing

    (newTask, maybeStats, maybeDrop) <- pushTaskAndCompleteIfNeeded task

    case maybeStatsCache of
        Nothing -> return ()
        Just statsCache -> do
            let
                newStatsCache = statsCache
                    { cacheCurrent = maybeStats
                    , cacheDrops =
                        maybe
                            (cacheDrops statsCache)
                            (\(ItemDrop itemDropMsg) ->
                                itemDropMsg : cacheDrops statsCache)
                            maybeDrop

                    }

            dataFile <- asks envHabiticaUserStatsCache
            liftIO $ Aeson.encodeFile dataFile newStatsCache

    liftIO $ putStrLn $ B.toString $ Aeson.encode newTask

modifyHook :: App ()
modifyHook = do
    (oldTaskJson, newTaskJson) <- liftIO $ (,) <$> getLine <*> getLine
    oldTask <- App.decodeTaskwarriorJSON oldTaskJson
    newTask <- App.decodeTaskwarriorJSON newTaskJson

    noteHasChanged <-
        case getTaskwarriorUUID newTask of
            Nothing -> return False
            Just uuid -> do
                noteFile <- getNotePath uuid
                noteExists <- liftIO $ Dir.doesFileExist noteFile
                if noteExists
                    then liftIO $ do
                        now <- Time.getCurrentTime
                        modTime <- Dir.getModificationTime noteFile
                        let timeDiff = Time.diffUTCTime now modTime
                        print timeDiff
                        return $ timeDiff <= 60
                    else
                        return False

    oldNoteAnnos <-
        filterNoteAnnotations True (taskAnnotations $ NT.unpack oldTask)

    newNoteAnnos <-
        filterNoteAnnotations True (taskAnnotations $ NT.unpack newTask)

    -- Turn the old and new Taskwarrior tasks into Habitica tasks; if
    -- they are equal, their notes haven't changed recently, and their
    -- note annotations haven't changed, then we don't need to push
    -- anything to Habitica.
    if toHabiticaTask oldTask == toHabiticaTask newTask
            && not noteHasChanged
            && oldNoteAnnos == newNoteAnnos
        then printTask newTask
        else do
            maybeDecodedFile <- getHabiticaStatsFromCacheFile

            statsCache <- case maybeDecodedFile of
                Nothing    ->
                    HabiticaUserStatsCache
                        <$> fetchStats
                        <*> return Nothing
                        <*> return []
                Just sCache -> return sCache

            (newerTask, maybeStats, maybeDrop) <- modifyTaskwarriorTask oldTask newTask

            let
                newDrops =
                    case maybeDrop of
                        Nothing -> cacheDrops statsCache
                        Just (ItemDrop itemDropMsg) ->
                            itemDropMsg : cacheDrops statsCache

                newStatsCache = statsCache
                    { cacheCurrent = maybeStats
                    , cacheDrops = newDrops
                    }

            dataFile <- asks envHabiticaUserStatsCache
            liftIO $ Aeson.encodeFile dataFile newStatsCache

            printTask newerTask
  where
    printTask = liftIO . putStrLn . B.toString . Aeson.encode

exitHook :: App ()
exitHook = do
    maybeStatsCache <- getHabiticaStatsFromCacheFile
    case maybeStatsCache of
        Nothing -> return ()
        Just statsCache -> do
            statsCachePath <- asks envHabiticaUserStatsCache
            liftIO $ do
                mapM_ putStrLn (getUserStatDiffs statsCache)
                tryJust (guard . IOError.isDoesNotExistError)
                    (Dir.removeFile statsCachePath) >>= const (return ())

sync :: App ()
sync = do
    verbose <- asks (argsVerbose . envArgs)
    Taskwarrior{..} <- asks envTaskwarrior
    habiticaTasks <- getHabiticaTasks
    (twOnlyTasks, twHabiticaSyncedTasks) <- getTaskwarriorTasks

    -- The task exists in Taskwarrior but not on Habitica, so we have to push it to Habitica
    -- and set the new Habitica ID in Taskwarrior
    forM_ twOnlyTasks $ \twTask -> do
        liftIO $ do
            putStrLn $ "Task: " <> T.unpack (taskText (NT.unpack twTask))
            putStrLn "    Status: Created in Taskwarrior."
            putStrLn "    Action: Pushing to Habitica and updating Habitica ID in Taskwarrior."
            putStrLn ""
        newTwTask <- pushTaskwarriorTask twTask
        void $ liftIO $ taskImport newTwTask

    -- Handle tasks that exist in Habitica
    let hTasks = tasksToHM habiticaTasks
    let twSyncedTasks = tasksToHM twHabiticaSyncedTasks
    let keys = Set.fromList $ HM.keys hTasks <> HM.keys twSyncedTasks
    userStats <- fetchStats
    foldM_ (\stats key ->
        case (HM.lookup key hTasks, HM.lookup key twSyncedTasks) of
            -- This should never occur, but if it somehow ever does, at least it has a descriptive error
            (Nothing, Nothing) ->
                throwError $ "Impossible state. A key was found with no corresponding value, " <>
                             "despite existing in one of the hashmaps. Key was: " <> show key

            -- The task exists on Habitica but not in Taskwarrior, so we have to import it to Taskwarrior
            (Just habiticaTask, Nothing) -> do
                liftIO $ do
                    putStrLn $ "Task: " <> T.unpack (taskText (NT.unpack habiticaTask))
                    putStrLn "    Status: Created on Habitica."
                    putStrLn "    Action: Importing into Taskwarrior."
                    putStrLn ""
                importAndUpdateNote (toTaskwarriorTask habiticaTask)
                return stats

            -- The task does not exist on Habitica, but it was previously synced with Taskwarrior.
            -- This means it used to exist on Habitica and was deleted, so set it to Deleted in Taskwarrior.
            (Nothing, Just (TaskwarriorTask twTask)) -> liftIO $ do
                putStrLn $ "Task: " <> T.unpack (taskText twTask)
                putStrLn "    Status: Deleted on Habitica."
                case taskStatus twTask of
                    TWCompleted -> do
                        putStrLn "    Action: Already completed in Taskwarrior. Leaving status as Completed. Unsetting Habitica ID."
                        void $ liftIO $ taskImport (TaskwarriorTask twTask{taskHabiticaId = Nothing})
                    _ -> do
                        putStrLn "    Action: Setting status to Deleted in Taskwarrior. Unsetting Habitica ID."
                        void $ liftIO $ taskImport (TaskwarriorTask
                            twTask{ taskStatus = TWDeleted
                                  , taskHabiticaId = Nothing
                                  })
                putStrLn ""
                return stats

            -- The task exists on both sides, so we'll take the most recently modified of the two
            -- and update the other to match
            (Just habiticaTask, Just taskwarriorTask) ->
                if Just habiticaTask == toHabiticaTask taskwarriorTask
                    then do
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
                        return $ Maybe.fromMaybe stats newStats
        ) userStats keys
  where
    tasksToHM :: (Newtype n, O n ~ Task status annotation) => [n] -> HashMap (Maybe UUID.UUID) n
    tasksToHM =
        foldr (\wrappedTask -> HM.insert
            (NT.unpack <$> taskHabiticaId (NT.unpack wrappedTask)) wrappedTask
        ) HM.empty

    updateFromNewest
        :: HabiticaTask
        -> TaskwarriorTask
        -> HabiticaUserStats
        -> App (Maybe HabiticaUserStats)
    updateFromNewest habiticaTask@(HabiticaTask hTask) taskwarriorTask@(TaskwarriorTask twTask) currentStats = do
        Taskwarrior{..} <- asks envTaskwarrior
        if taskModified hTask > taskModified twTask
            then do
                -- If the task was modified most recently on Habitica, we want to update
                -- the Taskwarrior task with the Habitica details and import it back
                -- into Taskwarrior
                liftIO $ putStrLn "    Action: Habitica task is most recently modified. Updating in Taskwarrior"
                importAndUpdateNote (updateTaskwarriorTask habiticaTask taskwarriorTask)
                return Nothing
            else do
                -- If the task was modified most recently on Taskwarrior, we can use the
                -- modifyTaskwarriorTask function to update Habitica by converting our Habitica
                -- task into a Taskwarrior task to fit the type signature
                liftIO $ putStrLn "    Action: Taskwarrior task is most recently modified. Updating on Habitica."
                (newTwTask, maybeStats, maybeDrop) <-
                    modifyTaskwarriorTask (toTaskwarriorTask habiticaTask) taskwarriorTask
                void $ liftIO $ taskImport newTwTask

                let
                    statsCache =
                        HabiticaUserStatsCache
                            currentStats
                            maybeStats
                            (maybe
                                []
                                (\(ItemDrop itemDropMsg) ->
                                    ["    " <> itemDropMsg])
                                maybeDrop)


                liftIO $ mapM_
                    (putStrLn . ("    " <>))
                    (getUserStatDiffs statsCache)

                return maybeStats

    bothSidesLog :: HabiticaTask -> TaskwarriorTask -> IO ()
    bothSidesLog habiticaTask taskwarriorTask = do
        putStrLn $ "Habitica Task:    " <> T.unpack (taskText (NT.unpack habiticaTask))
        putStrLn $ "Taskwarrior Task: " <> T.unpack (taskText (NT.unpack taskwarriorTask))
        putStrLn "    Status: Exists on both Habitica and Taskwarrior."

importAndUpdateNote :: TaskwarriorTask -> App ()
importAndUpdateNote twtask@(TaskwarriorTask task) = do
    Taskwarrior {taskImport} <- asks envTaskwarrior
    notePrefix <- asks envTaskNotePrefix
    noteDir <- asks envTaskNoteDir
    uuid <-
        case getTaskwarriorUUID twtask of
            Nothing          -> createUUID twtask
            Just (UUID uuid) -> return uuid

    -- Update the rawJson to have the new UUID we generated.
    -- If we did not generate a new UUID, this is basically a log n noop.
    let newJson = KM.insert "uuid" (Aeson.String (UUID.toText uuid)) (rawJson task)

    -- Remove any note annotations
    newAnnotations <-
        filterNoteAnnotations False (taskAnnotations task)

    noteFile <- getNotePath (UUID uuid)

    newTask <- case T.lines (T.strip $ taskNote task) of
        [] -> liftIO $ do
            -- There is no note or it is empty, so remove the file if it exists
            Dir.removeFile noteFile `catch` \(_ :: IOException) -> return ()
            -- Return the task with the altered annotations
            return $ TaskwarriorTask task
                { taskAnnotations = newAnnotations
                , rawJson = newJson
                }

        (x:_) -> liftIO $ do
            -- There is a note, so we create it.
            -- First create the notes directory if it doesn't exist.
            Dir.createDirectoryIfMissing True noteDir
            -- Write the note file
            T.writeFile noteFile (taskNote task)
            -- Return the task with an updated annotation
            currentTime <- Time.getCurrentTime
            let
                newAnno = Annotation
                    { annoEntry = timeToText taskwarriorTimeFormat currentTime
                    , annoDescription = notePrefix <> " " <> x
                    }
            return $ TaskwarriorTask task
                { taskAnnotations = newAnno : newAnnotations
                , rawJson = newJson
                }
    void $ liftIO $ taskImport newTask
  where
    createUUID twTask = do
        Taskwarrior{taskImport} <- asks envTaskwarrior
        outtxt <- liftIO $ T.words . T.pack <$> taskImport twTask
        let uuids = Maybe.mapMaybe UUID.fromText outtxt
        case uuids of
            [] ->
                throwError "Unable to generate a UUID for the imported task."
            (x:_) ->
                return x

getNotePath :: UUID -> App FilePath
getNotePath (UUID uuid) = do
    noteExtension <- asks envTaskNoteExtension
    noteDir <- asks envTaskNoteDir
    return (noteDir </> T.unpack (UUID.toText uuid) <.> noteExtension)

getTaskwarriorUUID :: TaskwarriorTask -> Maybe UUID
getTaskwarriorUUID =
    Aeson.parseMaybe (.: "uuid")
        . rawJson
        . NT.unpack

filterNoteAnnotations :: Bool -> [Annotation] -> App [Annotation]
filterNoteAnnotations keepNoteAnnotations annotations = do
    notePrefix <- asks envTaskNotePrefix
    return $
        filter
            ((==keepNoteAnnotations) . T.isPrefixOf notePrefix . T.strip . annoDescription)
            annotations
