{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module EIO.Time (
  -- * Effect
  Time,

  -- ** Handlers
  runTime,

  -- * Operations
  module RIO.Time,
  getCurrentTime,
  getCurrentTimeZone,
  getTimeZone,
  getZonedTime,
  utcToLocalZonedTime,
) where

import Effectful
import Effectful.Dispatch.Static
import RIO.Time hiding (
  getCurrentTime,
  getCurrentTimeZone,
  getTimeZone,
  getZonedTime,
  utcToLocalZonedTime,
 )
import RIO.Time qualified as T

data Time :: Effect
type instance DispatchOf Time = Static WithSideEffects
data instance StaticRep Time = Time

runTime :: (IOE :> es) => Eff (Time : es) a -> Eff es a
runTime = evalStaticRep Time

getCurrentTime :: (Time :> es) => Eff es UTCTime
getCurrentTime = unsafeEff_ T.getCurrentTime

getTimeZone :: (Time :> es) => UTCTime -> Eff es TimeZone
getTimeZone = unsafeEff_ . T.getTimeZone

getCurrentTimeZone :: (Time :> es) => Eff es TimeZone
getCurrentTimeZone = unsafeEff_ T.getCurrentTimeZone

getZonedTime :: (Time :> es) => Eff es ZonedTime
getZonedTime = unsafeEff_ T.getZonedTime

utcToLocalZonedTime :: (Time :> es) => UTCTime -> Eff es ZonedTime
utcToLocalZonedTime = unsafeEff_ . T.utcToLocalZonedTime
