module EIO.Vector.Unboxed.Unsafe (
  module RIO.Vector.Unboxed.Unsafe,
  unsafeFreeze,
  unsafeThaw,
  unsafeCopy,
) where

import Effectful (Eff, type (:>))
import Effectful.Prim (Prim, PrimStateEff)
import RIO.Vector.Unboxed (MVector, Unbox, Vector)
import RIO.Vector.Unboxed.Unsafe hiding (
  unsafeCopy,
  unsafeFreeze,
  unsafeThaw,
 )
import RIO.Vector.Unboxed.Unsafe qualified as V

-- NOTE: Lifting other functions is not worth the hard work.

-- | Lifted 'V.unsafeFreeze'.
unsafeFreeze :: (Unbox a, Prim :> es) => MVector PrimStateEff a -> Eff es (Vector a)
unsafeFreeze = V.unsafeFreeze

-- | Lifted 'V.unsafeThaw'.
unsafeThaw :: (Unbox a, Prim :> es) => Vector a -> Eff es (MVector PrimStateEff a)
unsafeThaw = V.unsafeThaw

-- | Lifted 'V.unsafeCopy'.
unsafeCopy :: (Unbox a, Prim :> es) => MVector PrimStateEff a -> Vector a -> Eff es ()
unsafeCopy = V.unsafeCopy
