{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module EIO.Process (
  -- * Effect
  ProcessE,

  -- ** Handlers
  runProcessE,

  -- * Process context
  withModifyEnvVars,
  lookupEnvFromContext,
  withWorkingDir,

  -- ** Actions
  resetExeCache,

  -- * Configuring
  proc,

  -- * Spawning (run child process)
  withProcessWait,
  withProcessWait_,
  withProcessTerm,
  withProcessTerm_,

  -- * Exec (replacing current process)
  exec,
  execSpawn,

  -- * Utilities
  doesExecutableExist,
  findExecutable,
  exeExtensions,

  -- * Reexports
  module RIO.Process,
) where

import Effectful
import Effectful.Dispatch.Static
import Effectful.Log.Static qualified as Log
import RIO
import RIO.Process hiding (
  doesExecutableExist,
  exeExtensions,
  exec,
  execSpawn,
  findExecutable,
  lookupEnvFromContext,
  proc,
  resetExeCache,
  withModifyEnvVars,
  withProcess,
  withProcessTerm,
  withProcessTerm_,
  withProcessWait,
  withProcessWait_,
  withProcess_,
  withWorkingDir,
 )
import RIO.Process qualified as RIO

data ProcessE :: Effect
type instance DispatchOf ProcessE = Static WithSideEffects
newtype instance StaticRep ProcessE = ProcessE ProcessContext

runProcessE :: (IOE :> es) => ProcessContext -> Eff (ProcessE : es) a -> Eff es a
runProcessE pc = evalStaticRep (ProcessE pc)

-- | Lifted 'RIO.resetExeCache'.
resetExeCache :: (ProcessE :> es) => Eff es ()
resetExeCache = do
  ProcessE pc <- getStaticRep
  unsafeEff_ . flip runReaderT pc $ RIO.resetExeCache

-- | Lifted 'RIO.withModifyEnvVars'.
withModifyEnvVars ::
  (ProcessE :> es) =>
  (EnvVars -> EnvVars) ->
  Eff es a ->
  Eff es a
withModifyEnvVars f inner = do
  ProcessE pc <- getStaticRep
  unsafeSeqUnliftIO $ \unlift ->
    flip runReaderT pc
      . RIO.withModifyEnvVars f
      . lift
      $ unlift inner

-- | Lifted 'RIO.lookupEnvFromContext'.
lookupEnvFromContext :: (ProcessE :> es) => Text -> Eff es (Maybe Text)
lookupEnvFromContext envName = do
  ProcessE pc <- getStaticRep
  unsafeEff_ . flip runReaderT pc $ RIO.lookupEnvFromContext envName

-- | Lifted 'RIO.withWorkingDir'.
withWorkingDir ::
  (ProcessE :> es) =>
  FilePath ->
  Eff es a ->
  Eff es a
withWorkingDir fp inner = do
  ProcessE pc <- getStaticRep
  unsafeSeqUnliftIO $ \unlift ->
    flip runReaderT pc
      . RIO.withWorkingDir fp
      . lift
      $ unlift inner

-- | Lifted 'RIO.proc'.
proc ::
  (ProcessE :> es, Log.Log :> es, HasCallStack) =>
  -- | command to run
  FilePath ->
  -- | command line arguments
  [String] ->
  (ProcessConfig () () () -> Eff es a) ->
  Eff es a
proc name0 args inner = do
  ProcessE pc <- getStaticRep
  Log.Log logFunc <- getStaticRep
  let !env = LoggedProcessContext pc logFunc
  unsafeSeqUnliftIO $ \unlift ->
    flip runReaderT env
      $ RIO.proc
        name0
        args
        (lift @(ReaderT LoggedProcessContext) . unlift . inner)

-- | Lifted 'RIO.withProcessWait'.
withProcessWait ::
  (ProcessE :> es) =>
  ProcessConfig stdin stdout stderr ->
  (Process stdin stdout stderr -> Eff es a) ->
  Eff es a
withProcessWait pc f =
  unsafeSeqUnliftIO $ \unlift ->
    RIO.withProcessWait pc (unlift . f)

-- | Lifted 'RIO.withProcessWait_'.
withProcessWait_ ::
  (ProcessE :> es) =>
  ProcessConfig stdin stdout stderr ->
  (Process stdin stdout stderr -> Eff es a) ->
  Eff es a
withProcessWait_ pc f =
  unsafeSeqUnliftIO $ \unlift ->
    RIO.withProcessWait_ pc (unlift . f)

-- | Lifted 'RIO.withProcessTerm'.
withProcessTerm ::
  (ProcessE :> es) =>
  ProcessConfig stdin stdout stderr ->
  (Process stdin stdout stderr -> Eff es a) ->
  Eff es a
withProcessTerm pc f =
  unsafeSeqUnliftIO $ \unlift ->
    RIO.withProcessTerm pc (unlift . f)

-- | Lifted 'RIO.withProcessTerm_'.
withProcessTerm_ ::
  (ProcessE :> es) =>
  ProcessConfig stdin stdout stderr ->
  (Process stdin stdout stderr -> Eff es a) ->
  Eff es a
withProcessTerm_ pc f =
  unsafeSeqUnliftIO $ \unlift ->
    RIO.withProcessTerm_ pc (unlift . f)

-- | Lifted 'RIO.exec'.
exec :: (ProcessE :> es, Log.Log :> es) => String -> [String] -> Eff es b
exec x y = do
  ProcessE pc <- getStaticRep
  Log.Log logFunc <- getStaticRep
  let !env = LoggedProcessContext pc logFunc
  unsafeEff_ . runRIO env $ RIO.exec x y

-- | Lifted 'RIO.execSpawn'.
execSpawn :: (ProcessE :> es, Log.Log :> es) => String -> [String] -> Eff es a
execSpawn cmd args = do
  ProcessE pc <- getStaticRep
  Log.Log logFunc <- getStaticRep
  let !env = LoggedProcessContext pc logFunc
  unsafeEff_ . runRIO env $ RIO.execSpawn cmd args

-- | Lifted 'RIO.doesExecutableExist'.
doesExecutableExist ::
  (ProcessE :> es) =>
  -- | Name of executable
  String ->
  Eff es Bool
doesExecutableExist s = do
  ProcessE pc <- getStaticRep
  unsafeEff_
    . flip runReaderT pc
    $ RIO.doesExecutableExist s

-- | Lifted 'RIO.findExecutable'.
findExecutable ::
  (ProcessE :> es) =>
  -- | Name of executable
  String ->
  -- | Full path to that executable on success
  Eff es (Either ProcessException FilePath)
findExecutable name = do
  ProcessE pc <- getStaticRep
  unsafeEff_
    . flip runReaderT pc
    $ RIO.findExecutable name

-- | Lifted 'RIO.exeExtensions'.
exeExtensions ::
  (ProcessE :> es) =>
  Eff es [String]
exeExtensions = do
  ProcessE pc <- getStaticRep
  unsafeEff_
    . flip runReaderT pc
    $ RIO.exeExtensions
