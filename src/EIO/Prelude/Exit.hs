{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module EIO.Prelude.Exit (
  -- * Effect
  ProcessExit,

  -- ** Handlers
  runProcessExit,

  -- ** opreations
  exitFailure,
  exitSuccess,
  exitWith,
  ExitCode (..),
) where

import Effectful
import Effectful.Dispatch.Static
import RIO (ExitCode (..))
import RIO qualified

-- | An effect for exiting the process.
data ProcessExit :: Effect

type instance DispatchOf ProcessExit = Static WithSideEffects
data instance StaticRep ProcessExit = ProcessExit

-- | Run a 'ProcessExit' effect by exiting the process.
runProcessExit :: (IOE :> es) => Eff (ProcessExit : es) a -> Eff es a
runProcessExit = evalStaticRep ProcessExit

-- | Lifted 'RIO.exitFailure'.
exitFailure :: (ProcessExit :> es) => Eff es a
exitFailure = unsafeEff_ RIO.exitFailure

-- | Lifted 'RIO.exitSuccess'.
exitSuccess :: (ProcessExit :> es) => Eff es a
exitSuccess = unsafeEff_ RIO.exitSuccess

-- | Lifted 'RIO.exitWith'.
exitWith :: (ProcessExit :> es) => ExitCode -> Eff es a
exitWith = unsafeEff_ . RIO.exitWith
