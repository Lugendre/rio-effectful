module EIO.Prelude.IORef (
  IORef,
  newIORef,
  readIORef,
  writeIORef,
  modifyIORef,
  modifyIORef',
  atomicModifyIORef,
  atomicModifyIORef',
  atomicWriteIORef,
  mkWeakIORef,
) where

import Effectful
import System.Mem.Weak (Weak)
import UnliftIO.IORef (IORef)
import UnliftIO.IORef qualified as I

-- | Lifted 'I.newIORef'.
newIORef :: (IOE :> es) => a -> Eff es (IORef a)
newIORef = I.newIORef

-- | Lifted 'I.readIORef'.
readIORef :: (IOE :> es) => IORef a -> Eff es a
readIORef = I.readIORef

-- | Lifted 'I.writeIORef'.
writeIORef :: (IOE :> es) => IORef a -> a -> Eff es ()
writeIORef = I.writeIORef

-- | Lifted 'I.modifyIORef'.
modifyIORef :: (IOE :> es) => IORef a -> (a -> a) -> Eff es ()
modifyIORef = I.modifyIORef

-- | Lifted 'I.modifyIORef''.
modifyIORef' :: (IOE :> es) => IORef a -> (a -> a) -> Eff es ()
modifyIORef' = I.modifyIORef'

-- | Lifted 'I.atomicModifyIORef'.
atomicModifyIORef :: (IOE :> es) => IORef a -> (a -> (a, b)) -> Eff es b
atomicModifyIORef = I.atomicModifyIORef

-- | Lifted 'I.atomicModifyIORef''.
atomicModifyIORef' :: (IOE :> es) => IORef a -> (a -> (a, b)) -> Eff es b
atomicModifyIORef' = I.atomicModifyIORef'

-- | Lifted 'I.atomicWriteIORef'.
atomicWriteIORef :: (IOE :> es) => IORef a -> a -> Eff es ()
atomicWriteIORef = I.atomicWriteIORef

-- | Unlifted 'I.mkWeakIORef'.
mkWeakIORef :: (IOE :> es) => IORef a -> Eff es () -> Eff es (Weak (IORef a))
mkWeakIORef = I.mkWeakIORef
