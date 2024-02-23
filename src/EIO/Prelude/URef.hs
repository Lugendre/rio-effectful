module EIO.Prelude.URef (
  URef,
  IOURef,
  newURef,
  readURef,
  writeURef,
  modifyURef,
) where

import Effectful
import Effectful.Prim
import RIO hiding (IOURef, modifyURef, newURef, readURef, writeURef)
import RIO qualified

-- | Helpful type synonym for using a URef from an Eff-based stack.
type IOURef = URef PrimStateEff

-- | Lifted 'RIO.newURef'.
newURef :: (Prim :> es, Unbox a) => a -> Eff es (URef PrimStateEff a)
newURef = RIO.newURef

-- | Lifted 'RIO.readURef'.
readURef :: (Prim :> es, Unbox a) => URef PrimStateEff a -> Eff es a
readURef = RIO.readURef

-- | Lifted 'RIO.writeURef'.
writeURef :: (Prim :> es, Unbox a) => URef PrimStateEff a -> a -> Eff es ()
writeURef = RIO.writeURef

-- | Lifted 'RIO.modifyURef'.
modifyURef :: (Prim :> es, Unbox a) => URef PrimStateEff a -> (a -> a) -> Eff es ()
modifyURef = RIO.modifyURef
