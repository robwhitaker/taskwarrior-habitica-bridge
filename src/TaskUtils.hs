{-# LANGUAGE TypeFamilies #-}

module TaskUtils where

import           Control.Newtype.Generics (Newtype, O)
import qualified Control.Newtype.Generics as NT

import           Data.Aeson.Types         (Value, emptyObject)

import           Types

convertTask :: (Newtype n1, Newtype n2, O n1 ~ Task, O n2 ~ Task) => (Task -> Task) -> n1 -> Value -> n2
convertTask f task json = NT.pack $ f (NT.unpack task) {rawJson = json}

toHabiticaTask :: TaskwarriorTask -> HabiticaTask
toHabiticaTask task = convertTask statusFixer task emptyObject
  where
    statusFixer :: Task -> Task
    statusFixer inTask =
        inTask {
            taskStatus =
                case taskStatus inTask of
                    Waiting -> Pending
                    other   -> other
        }

toTaskwarriorTask :: HabiticaTask -> TaskwarriorTask
toTaskwarriorTask task = convertTask id task emptyObject

updateTaskwarriorTask :: HabiticaTask -> TaskwarriorTask -> TaskwarriorTask
updateTaskwarriorTask hTask twTask = convertTask id hTask (rawJson $ NT.unpack twTask)
  where
    statusFixer :: Task -> Task
    statusFixer inTask =
        inTask {
            taskStatus =
                case (taskStatus inTask, taskStatus (NT.unpack twTask)) of
                    (Pending, Waiting) -> Waiting
                    (other, _)         -> other
        }

-- This technically ignores the Habitica task
-- as leaving out the origin JSON fields is okay
-- since the fields are optional according to the
-- API.
updateHabiticaTask :: TaskwarriorTask -> HabiticaTask -> HabiticaTask
updateHabiticaTask twTask hTask = toHabiticaTask twTask
