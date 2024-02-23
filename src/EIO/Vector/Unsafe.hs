module EIO.Vector.Unsafe (
  module RIO.Vector.Unsafe,
  unsafeFreeze,
  unsafeThaw,
  unsafeCopy,
) where

import Data.Vector.Generic (Mutable)
import Effectful (Eff, type (:>))
import Effectful.Prim (Prim, PrimStateEff)
import RIO.Vector.Unsafe hiding (
  unsafeCopy,
  unsafeFreeze,
  unsafeThaw,
 )
import RIO.Vector.Unsafe qualified as V

-- NOTE: Lifting other functions and 'Vector' class is not worth the hard work.

-- | Lifted 'V.unsafeFreeze'.
unsafeFreeze :: (Prim :> es, Vector v a) => Mutable v PrimStateEff a -> Eff es (v a)
unsafeFreeze = V.unsafeFreeze

-- | Lifted 'V.unsafeThaw'.
unsafeThaw :: (Prim :> es, Vector v a) => v a -> Eff es (Mutable v PrimStateEff a)
unsafeThaw = V.unsafeThaw

-- | Lifted 'V.unsafeCopy'.
unsafeCopy :: (Prim :> es, Vector v a) => Mutable v PrimStateEff a -> v a -> Eff es ()
unsafeCopy = V.unsafeCopy
