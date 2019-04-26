{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified System.Process as Process
import qualified Data.Text as T
import Data.Text (Text)
import Data.Aeson (eitherDecode, encode)
import Data.Aeson.Types (emptyObject)
import Data.String (fromString)
import qualified Data.ByteString.Lazy.UTF8 as B
import qualified Control.Newtype.Generics as NT
import qualified Network.HTTP.Req as Req
import qualified Data.HashMap.Strict as HM
import qualified Data.UUID as UUID
import Control.Concurrent (threadDelay)

import Types
import Web
import TaskUtils

-- Taskwarrior command without STDIN
twCmd :: [String] -> (String -> a) -> IO a
twCmd cmd f =
    twCmd' cmd "" f

-- Taskwarrior command with STDIN
twCmd' :: [String] -> String -> (String -> a) -> IO a
twCmd' cmd stdin f =
    Process.readProcessWithExitCode "task" cmd stdin
        >>= return . (\(_, result, _) -> f result)

twGet :: String -> IO Text
twGet str =
    twCmd [ "_get", str ] (T.strip . T.pack)

twExport :: [String] -> IO [TaskwarriorTask]
twExport filters =
    twCmd (filters ++ [ "export" ]) (either error id . eitherDecode . fromString)

twImport :: TaskwarriorTask -> IO Text
twImport task =
    twCmd' [ "import", "-" ] (B.toString $ encode task) (T.strip . T.pack)

getHabiticaHeaders :: IO HabiticaHeaders
getHabiticaHeaders = do
    -- TODO: this assume (a) task is installed and (b) there will be no error
    habiticaId <- twGet "rc.habitica.user_id"
    habiticaApiKey <- twGet "rc.habitica.api_key"
    case habiticaHeaders habiticaId habiticaApiKey of
        Nothing ->
            error "you goofed bruh"

        Just headers ->
            return headers

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
    completed <- runHabiticaReq (habiticaGetTasks headers (Just "completedTodos")) >>= habiticaResponseHandle
    todos <- runHabiticaReq (habiticaGetTasks headers (Just "todos")) >>= habiticaResponseHandle
    return $ todos <> completed

habiticaResponseHandle :: HabiticaResponse a -> IO a
habiticaResponseHandle (HttpException e) =
    error $ "Something went wrong with the network request: " <> show e
habiticaResponseHandle (ErrorResponse errText errMessage) =
    error $ T.unpack $ errText <> ": " <> errMessage
habiticaResponseHandle (ParseError errText) =
    error $ "Something went wrong while parsing the response from Habitica: " <> errText
habiticaResponseHandle (DataResponse response) =
    return response

addToHabitica :: HabiticaHeaders -> TaskwarriorTask -> IO ()
addToHabitica headers twTask@(TaskwarriorTask task) = do
    let habiticaTask = toHabiticaTask twTask
    -- Stagger the Habitica request so the server doesn't choke on request overload
    -- and do nothing
    threadDelay 1000000
    -- Create the taskwarrior task on Habitica and retrieve the id of the returned task
    (HabiticaTask (Task { taskHabiticaId })) <- runHabiticaReq
        (habiticaCreateOrUpdateRequest headers habiticaTask) >>= habiticaResponseHandle
    -- Update the task in taskwarrior with the new id
    twImport (TaskwarriorTask $ task { taskHabiticaId = taskHabiticaId })
    return ()

updateHabitica :: HabiticaHeaders -> HabiticaTask -> HasStatusChange -> IO ()
updateHabitica headers hTask@(HabiticaTask task) hasStatusChange = do
    -- Stagger the Habitica request so the server doesn't choke on request overload
    -- and do nothing
    threadDelay 1000000
    -- Update the task on Habitica
    runHabiticaReq (habiticaCreateOrUpdateRequest headers hTask) >>= habiticaResponseHandle
    -- If the status changed, we need to "score" the task to change it on Habitica
    if hasStatusChange then
        do
            -- Sleep again before the next request
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
    else
        return ()


main :: IO ()
main = do
    headers <- getHabiticaHeaders
    habiticaTasks <- getHabiticaTasks headers
    (twOnlyTasks, twHabiticaSyncedTasks) <- getTaskwarriorTasks
    let hTasks = foldr (\hTask@(HabiticaTask task) taskMap ->
                let key = maybe "" UUID.toText $ NT.unpack <$> taskHabiticaId task
                in
                    HM.insert key hTask taskMap
            ) HM.empty habiticaTasks
    let twTasks = foldr (\twTask@(TaskwarriorTask task) taskMap ->
                let key = maybe "" UUID.toText $ NT.unpack <$> taskHabiticaId task
                in
                    HM.insert key twTask taskMap
            ) HM.empty twHabiticaSyncedTasks
    let habiticaOnlyTasks = HM.difference hTasks twTasks
    let deletedFromHabitica = HM.difference twTasks hTasks
    let taskUpdates = HM.mapMaybe id $
            HM.intersectionWith (\h@(HabiticaTask hTask) t@(TaskwarriorTask twTask) ->
                if hTask == twTask then
                    Nothing
                else if taskModified hTask > taskModified twTask then
                    -- Habitica was updated more recently, so update Taskwarrior
                    Just $ UpdateTaskwarrior $ updateTaskwarriorTask h t
                else
                    -- Taskwarrior was updated more recently, so update Habitica
                    Just $ UpdateHabitica (updateHabiticaTask t h) (taskStatus hTask /= taskStatus twTask)
            ) hTasks twTasks


    let changes =
            (map Update $ HM.elems taskUpdates) <>
            (map AddToHabitica twOnlyTasks) <>
            (map AddToTaskwarrior $ HM.elems habiticaOnlyTasks) <>
            (map DeleteFromTaskwarrior $ HM.elems deletedFromHabitica)
    mapM_ (\change ->
        case change of
            AddToTaskwarrior habiticaTask ->
                twImport (toTaskwarriorTask habiticaTask) >> return ()

            DeleteFromTaskwarrior taskwarriorTask@(TaskwarriorTask task) ->
                twImport (TaskwarriorTask $ task { taskStatus = Deleted }) >> return ()


            AddToHabitica taskwarriorTask ->
                addToHabitica headers taskwarriorTask

            Update (UpdateTaskwarrior taskwarriorTask) ->
                twImport taskwarriorTask >> return ()

            Update (UpdateHabitica habiticaTask hasStatusChange) ->
                updateHabitica headers habiticaTask hasStatusChange
        ) changes
