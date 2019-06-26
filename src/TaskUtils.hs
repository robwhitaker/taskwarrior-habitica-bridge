{-# LANGUAGE TypeFamilies #-}

module TaskUtils
    (
    -- * Task conversions
      toHabiticaTask
    , toTaskwarriorTask

    -- * Task updates
    , updateHabiticaTask
    , updateTaskwarriorTask
    ) where

import           Control.Newtype.Generics (Newtype, O)
import qualified Control.Newtype.Generics as NT

import           Data.Aeson.Types         (Object)
import qualified Data.HashMap.Strict      as HM
import qualified Data.Maybe               as Maybe

import           Types

-- Utility function to convert one task type to another

convertTask
    :: (Newtype n1, Newtype n2, O n1 ~ Task a, O n2 ~ Task b)
    => (Task a -> Maybe (Task b)) -> n1 -> Object -> Maybe n2
convertTask f task json =
    f (NT.unpack task) >>= (\newTask -> Just $ NT.pack (newTask {rawJson = json}))

-- Specific task conversions

toHabiticaTask :: TaskwarriorTask -> Maybe HabiticaTask
toHabiticaTask task = convertTask statusFixer task HM.empty
  where
    statusFixer :: Task TWTaskStatus -> Maybe (Task HTaskStatus)
    statusFixer inTask =
        case taskStatus inTask of
            TWWaiting   -> Just inTask { taskStatus = HPending }
            TWPending   -> Just inTask { taskStatus = HPending }
            TWCompleted -> Just inTask { taskStatus = HCompleted }
            TWDeleted   -> Just inTask { taskStatus = HDeleted }
            TWRecurring -> Nothing

toTaskwarriorTask :: HabiticaTask -> TaskwarriorTask
toTaskwarriorTask task = Maybe.fromJust $ convertTask statusFixer task HM.empty
  where
    statusFixer :: Task HTaskStatus -> Maybe (Task TWTaskStatus)
    statusFixer inTask =
        case taskStatus inTask of
            HPending   -> Just inTask { taskStatus = TWPending }
            HCompleted -> Just inTask { taskStatus = TWCompleted }
            HDeleted   -> Just inTask { taskStatus = TWDeleted }

-- Updating one type of task from another

updateTaskwarriorTask :: HabiticaTask -> TaskwarriorTask -> TaskwarriorTask
updateTaskwarriorTask hTask twTask = Maybe.fromJust $
    convertTask statusFixer hTask (rawJson $ NT.unpack twTask)
  where
    statusFixer :: Task HTaskStatus -> Maybe (Task TWTaskStatus)
    statusFixer inTask =
        case (taskStatus inTask, taskStatus (NT.unpack twTask)) of
            -- A Taskwarrior recurring task has no equivalent representation in Habitica
            -- and should only exist as a "template" to create new tasks on the Taskwarrior
            -- side. If we hit this path, something went very wrong (e.g. the user manually
            -- entered a Habitica ID into a recurring task). This branch "resets" the recurring
            -- task by returning the original task without the Habitica updates and removing the
            -- Habitica ID.
            (_, TWRecurring) ->
                let
                    (TaskwarriorTask task) = twTask
                in
                    Just task { taskHabiticaId = Nothing }
            (HPending, TWWaiting) -> Just inTask { taskStatus = TWWaiting }
            (HPending, _)         -> Just inTask { taskStatus = TWPending }
            (HCompleted, _)       -> Just inTask { taskStatus = TWCompleted }
            (HDeleted, _)         -> Just inTask { taskStatus = TWDeleted }

-- This technically ignores the Habitica task
-- as leaving out the origin JSON fields is okay
-- since the fields are optional according to the
-- API.
updateHabiticaTask :: TaskwarriorTask -> HabiticaTask -> Maybe HabiticaTask
updateHabiticaTask twTask _ = toHabiticaTask twTask
