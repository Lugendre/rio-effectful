module EIO.Vector.Storable.Unsafe (
  module RIO.Vector.Storable.Unsafe,
  unsafeFreeze,
  unsafeThaw,
  unsafeCopy,
  unsafeWith,
) where

import Effectful (Eff, IOE, type (:>))
import Effectful.Prim (Prim, PrimStateEff)
import Foreign (Ptr)
import RIO.Vector.Storable (MVector, Storable, Vector)
import RIO.Vector.Storable.Unsafe hiding (
  unsafeCopy,
  unsafeFreeze,
  unsafeThaw,
  unsafeWith,
 )
import RIO.Vector.Storable.Unsafe qualified as V

-- NOTE: Lifting other functions is not worth the hard work.

-- | Lifted 'V.unsafeFreeze'.
unsafeFreeze :: (Storable a, Prim :> es) => MVector PrimStateEff a -> Eff es (Vector a)
unsafeFreeze = V.unsafeFreeze

-- | Lifted 'V.unsafeThaw'.
unsafeThaw :: (Storable a, Prim :> es) => Vector a -> Eff es (MVector PrimStateEff a)
unsafeThaw = V.unsafeThaw

-- | Lifted 'V.unsafeCopy'.
unsafeCopy :: (Storable a, Prim :> es) => MVector PrimStateEff a -> Vector a -> Eff es ()
unsafeCopy = V.unsafeCopy

-- | Lifted 'V.unsafeWith'.
unsafeWith :: (IOE :> es, Storable a) => Vector a -> (Ptr a -> Eff es b) -> Eff es b
unsafeWith = V.unsafeWith
