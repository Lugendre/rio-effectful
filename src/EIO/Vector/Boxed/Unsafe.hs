module EIO.Vector.Boxed.Unsafe (
  module RIO.Vector.Boxed.Unsafe,
  unsafeFreeze,
  unsafeThaw,
  unsafeCopy,
) where

import Effectful (Eff, type (:>))
import Effectful.Prim (Prim, PrimStateEff)
import RIO.Vector.Boxed (MVector, Vector)
import RIO.Vector.Boxed.Unsafe hiding (
  unsafeCopy,
  unsafeFreeze,
  unsafeThaw,
 )
import RIO.Vector.Boxed.Unsafe qualified as V

-- NOTE: Lifting other functions is not worth the hard work.

-- | Lifted 'V.unsafeFreeze'.
unsafeFreeze :: (Prim :> es) => MVector PrimStateEff a -> Eff es (Vector a)
unsafeFreeze = V.unsafeFreeze

-- | Lifted 'V.unsafeThaw'.
unsafeThaw :: (Prim :> es) => Vector a -> Eff es (MVector PrimStateEff a)
unsafeThaw = V.unsafeThaw

-- | Lifted 'V.unsafeCopy'.
unsafeCopy :: (Prim :> es) => MVector PrimStateEff a -> Vector a -> Eff es ()
unsafeCopy = V.unsafeCopy
