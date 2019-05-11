{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Control.Concurrent        (threadDelay)
import qualified Control.Newtype.Generics  as NT

import           Data.Aeson                (eitherDecode, encode)
import           Data.Aeson.Types          (emptyObject)
import qualified Data.ByteString.Lazy.UTF8 as B
import qualified Data.HashMap.Strict       as HM
import           Data.String               (fromString)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.UUID                 as UUID

import qualified Network.HTTP.Req          as Req
import qualified System.Process            as Process

import           TaskUtils
import           Types
import           Web

-- Taskwarrior command without STDIN
twCmd :: [String] -> (String -> a) -> IO a
twCmd cmd f = twCmd' cmd "" f

-- Taskwarrior command with STDIN
twCmd' :: [String] -> String -> (String -> a) -> IO a
twCmd' cmd stdin f =
    Process.readProcessWithExitCode "task" cmd stdin >>=
    return . (\(_, result, _) -> f result)

twGet :: String -> IO Text
twGet str = twCmd ["_get", str] (T.strip . T.pack)

twExport :: [String] -> IO [TaskwarriorTask]
twExport filters =
    twCmd (filters ++ ["export"]) (either error id . eitherDecode . fromString)

twImport :: TaskwarriorTask -> IO Text
twImport task = twCmd' ["import", "-"] (B.toString $ encode task) (T.strip . T.pack)

getHabiticaHeaders :: IO HabiticaHeaders
getHabiticaHeaders
    -- TODO: this assume (a) task is installed and (b) there will be no error
 = do
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
    completed <-
        runHabiticaReq (habiticaGetTasks headers (Just "completedTodos")) >>=
        habiticaResponseHandle
    -- TODO: This could be made more efficient by just doing one request and filtering
    -- out the tasks we don't want, but the decoders would have to be updated to handle
    -- different shapes.
    todos <-
        runHabiticaReq (habiticaGetTasks headers (Just "todos")) >>=
        habiticaResponseHandle
    dailies <-
        runHabiticaReq (habiticaGetTasks headers (Just "dailys")) >>=
        habiticaResponseHandle
    -- We don't care about rewards or habits
    return $ todos <> dailies <> completed

habiticaResponseHandle :: HabiticaResponse a -> IO a
habiticaResponseHandle (HttpException e) =
    error $ "Something went wrong with the network request: " <> show e
habiticaResponseHandle (ErrorResponse errText errMessage) =
    error $ T.unpack $ errText <> ": " <> errMessage
habiticaResponseHandle (ParseError errText) =
    error $ "Something went wrong while parsing the response from Habitica: " <> errText
habiticaResponseHandle (DataResponse response) = return response

addToHabitica :: HabiticaHeaders -> TaskwarriorTask -> IO ()
addToHabitica headers twTask@(TaskwarriorTask task) = do
    let habiticaTask = toHabiticaTask twTask
    -- Stagger the Habitica request so the server doesn't choke on request overload
    -- and do nothing
    threadDelay 1000000
    -- Create the taskwarrior task on Habitica and retrieve the id of the returned task
    (HabiticaTask (Task {taskHabiticaId})) <-
        runHabiticaReq (habiticaCreateOrUpdateRequest headers habiticaTask) >>=
        habiticaResponseHandle
    -- Update the task in taskwarrior with the new id
    twImport (TaskwarriorTask $ task {taskHabiticaId = taskHabiticaId})
    return ()

updateHabitica :: HabiticaHeaders -> HabiticaTask -> HasStatusChange -> IO ()
updateHabitica headers hTask@(HabiticaTask task) hasStatusChange
    -- Stagger the Habitica request so the server doesn't choke on request overload
    -- and do nothing
 = do
    threadDelay 1000000
    -- Update the task on Habitica
    runHabiticaReq (habiticaCreateOrUpdateRequest headers hTask) >>=
        habiticaResponseHandle
    -- If the status changed, we need to "score" the task to change it on Habitica
    if hasStatusChange
            -- Sleep again before the next request
        then do
            threadDelay 1000000
            let taskId =
                    maybe
                        (error "Trying to update a Habitica task with no ID.")
                        id
                        (taskHabiticaId task)
            case (taskStatus task) of
                Pending -> do
                    runHabiticaReq' (habiticaScoreTask headers taskId Down)
                Completed -> do
                    runHabiticaReq' (habiticaScoreTask headers taskId Up)
                Deleted -> do
                    runHabiticaReq' (habiticaDeleteTask headers taskId)
        else return ()

main :: IO ()
main = do
    headers <- getHabiticaHeaders
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
            (map Update $ HM.elems taskUpdates) <> (map AddToHabitica twOnlyTasks) <>
            (map AddToTaskwarrior $ HM.elems habiticaOnlyTasks) <>
            (map DeleteFromTaskwarrior $ HM.elems deletedFromHabitica)
    mapM_
        (\change ->
             case change of
                 AddToTaskwarrior habiticaTask ->
                     twImport (toTaskwarriorTask habiticaTask) >> return ()
                 DeleteFromTaskwarrior taskwarriorTask@(TaskwarriorTask task) ->
                     twImport (TaskwarriorTask $ task {taskStatus = Deleted}) >>
                     return ()
                 AddToHabitica taskwarriorTask -> addToHabitica headers taskwarriorTask
                 Update (UpdateTaskwarrior taskwarriorTask) ->
                     twImport taskwarriorTask >> return ()
                 Update (UpdateHabitica habiticaTask hasStatusChange) ->
                     updateHabitica headers habiticaTask hasStatusChange)
        changes
