{-# LANGUAGE TypeFamilies #-}

module TaskUtils where

import           Control.Newtype.Generics (Newtype, O)
import qualified Control.Newtype.Generics as NT

import           Data.Aeson.Types         (Value, emptyObject)
import           Data.Maybe               (fromJust)

import           Types

convertTask
    :: (Newtype n1, Newtype n2, O n1 ~ Task a, O n2 ~ Task b)
    => (Task a -> Maybe (Task b)) -> n1 -> Value -> Maybe n2
convertTask f task json =
    f (NT.unpack task) >>= (\newTask -> Just $ NT.pack (newTask {rawJson = json}))

toHabiticaTask :: TaskwarriorTask -> Maybe HabiticaTask
toHabiticaTask task = convertTask statusFixer task emptyObject
  where
    statusFixer :: Task TWTaskStatus -> Maybe (Task HTaskStatus)
    statusFixer inTask =
        case taskStatus inTask of
            TWWaiting   -> Just inTask { taskStatus = HPending }
            TWPending   -> Just inTask { taskStatus = HPending }
            TWCompleted -> Just inTask { taskStatus = HCompleted }
            TWDeleted   -> Just inTask { taskStatus = HDeleted }

toTaskwarriorTask :: HabiticaTask -> TaskwarriorTask
toTaskwarriorTask task = fromJust $ convertTask statusFixer task emptyObject
  where
    statusFixer :: Task HTaskStatus -> Maybe (Task TWTaskStatus)
    statusFixer inTask =
        case taskStatus inTask of
            HPending   -> Just inTask { taskStatus = TWPending }
            HCompleted -> Just inTask { taskStatus = TWCompleted }
            HDeleted   -> Just inTask { taskStatus = TWDeleted }

updateTaskwarriorTask :: HabiticaTask -> TaskwarriorTask -> TaskwarriorTask
updateTaskwarriorTask hTask twTask = fromJust $ convertTask statusFixer hTask (rawJson $ NT.unpack twTask)
  where
    statusFixer :: Task HTaskStatus -> Maybe (Task TWTaskStatus)
    statusFixer inTask =
        case (taskStatus inTask, taskStatus (NT.unpack twTask)) of
            (HPending, TWWaiting) -> Just inTask { taskStatus = TWWaiting }
            (HPending, _)         -> Just inTask { taskStatus = TWPending }
            (HCompleted, _)       -> Just inTask { taskStatus = TWCompleted }
            (HDeleted, _)         -> Just inTask { taskStatus = TWDeleted }

-- This technically ignores the Habitica task
-- as leaving out the origin JSON fields is okay
-- since the fields are optional according to the
-- API.
updateHabiticaTask :: TaskwarriorTask -> HabiticaTask -> Maybe HabiticaTask
updateHabiticaTask twTask hTask = toHabiticaTask twTask
