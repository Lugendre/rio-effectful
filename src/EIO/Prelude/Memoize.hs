module EIO.Prelude.Memoize (
  Memoized,
  runMemoized,
  memoizeRef,
  memoizeMVar,
) where

import Effectful
import UnliftIO.Memoize hiding (memoizeMVar, memoizeRef, runMemoized)
import UnliftIO.Memoize qualified as M

-- | Lifted 'M.runMemoized'.
runMemoized :: (IOE :> es) => Memoized a -> Eff es a
runMemoized = M.runMemoized
{-# INLINE runMemoized #-}

-- | Lifted 'M.memoizeRef'.
memoizeRef :: (IOE :> es) => Eff es a -> Eff es (Memoized a)
memoizeRef = M.memoizeRef

-- | Lifted 'M.memoizeMVar'.
memoizeMVar :: (IOE :> es) => Eff es a -> Eff es (Memoized a)
memoizeMVar = M.memoizeMVar
