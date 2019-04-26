{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module TaskUtils where

import Types

import Control.Newtype.Generics (Newtype, O)
import qualified Control.Newtype.Generics as NT
import Data.Aeson.Types (emptyObject, Value)

convertTask
    :: (Newtype n1, Newtype n2, O n1 ~ Task, O n2 ~ Task) =>
       n1 -> Value -> n2
convertTask task json =
    NT.pack $ (NT.unpack task) { rawJson = json }

toHabiticaTask :: TaskwarriorTask -> HabiticaTask
toHabiticaTask task =
    convertTask task emptyObject

toTaskwarriorTask :: HabiticaTask -> TaskwarriorTask
toTaskwarriorTask task =
    convertTask task emptyObject

updateTaskwarriorTask :: HabiticaTask -> TaskwarriorTask -> TaskwarriorTask
updateTaskwarriorTask hTask twTask =
    convertTask hTask (rawJson $ NT.unpack twTask)

-- This technically ignores the Habitica task
-- as leaving out the origin JSON fields is okay
-- since the fields are optional according to the
-- API.
updateHabiticaTask :: TaskwarriorTask -> HabiticaTask -> HabiticaTask
updateHabiticaTask twTask hTask =
    toHabiticaTask twTask
