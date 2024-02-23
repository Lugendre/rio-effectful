{-# LANGUAGE NoImplicitPrelude #-}

module EIO.Deque (
  -- * Types
  Deque,
  UDeque,
  SDeque,
  BDeque,

  -- * Operations
  newDeque,
  getDequeSize,
  popFrontDeque,
  popBackDeque,
  pushFrontDeque,
  pushBackDeque,
  foldlDeque,
  foldrDeque,
  dequeToList,
  dequeToVector,
  freezeDeque,

  -- * Inference helpers
  asUDeque,
  asSDeque,
  asBDeque,
) where

import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as V
import Effectful (Eff, type (:>))
import Effectful.Prim (Prim, PrimStateEff)
import RIO (Int, Maybe)
import RIO.Deque hiding (
  dequeToList,
  dequeToVector,
  foldlDeque,
  foldrDeque,
  freezeDeque,
  getDequeSize,
  newDeque,
  popBackDeque,
  popFrontDeque,
  pushBackDeque,
  pushFrontDeque,
 )
import RIO.Deque qualified as D

-- | Lifted 'D.newDeque'.
newDeque ::
  (V.MVector v a, Prim :> es) =>
  Eff es (Deque v PrimStateEff a)
newDeque = D.newDeque
{-# INLINE newDeque #-}

-- | Lifted 'D.getDequeSize'.
getDequeSize :: (Prim :> es) => Deque v (PrimStateEff) a -> Eff es Int
getDequeSize = D.getDequeSize
{-# INLINE getDequeSize #-}

-- | Lifted 'D.popFrontDeque'.
popFrontDeque ::
  (V.MVector v a, Prim :> es) =>
  Deque v (PrimStateEff) a ->
  Eff es (Maybe a)
popFrontDeque = D.popFrontDeque
{-# INLINE popFrontDeque #-}

-- | Lifted 'D.popBackDeque'.
popBackDeque ::
  (V.MVector v a, Prim :> es) =>
  Deque v (PrimStateEff) a ->
  Eff es (Maybe a)
popBackDeque = D.popBackDeque
{-# INLINE popBackDeque #-}

-- | Lifted 'D.pushFrontDeque'.
pushFrontDeque ::
  (V.MVector v a, Prim :> es) =>
  Deque v (PrimStateEff) a ->
  a ->
  Eff es ()
pushFrontDeque = D.pushFrontDeque
{-# INLINE pushFrontDeque #-}

-- | Lifted 'D.pushBackDeque'.
pushBackDeque ::
  (V.MVector v a, Prim :> es) =>
  Deque v (PrimStateEff) a ->
  a ->
  Eff es ()
pushBackDeque = D.pushBackDeque
{-# INLINE pushBackDeque #-}

-- | Lifted 'D.foldlDeque'.
foldlDeque ::
  (V.MVector v a, Prim :> es) =>
  (acc -> a -> Eff es acc) ->
  acc ->
  Deque v (PrimStateEff) a ->
  Eff es acc
foldlDeque = D.foldlDeque

-- | Lifted 'D.foldrDeque'.
foldrDeque ::
  (V.MVector v a, Prim :> es) =>
  (a -> acc -> Eff es acc) ->
  acc ->
  Deque v (PrimStateEff) a ->
  Eff es acc
foldrDeque = D.foldrDeque

-- | Lifted 'D.dequeToList'.
dequeToList ::
  (V.MVector v a, Prim :> es) =>
  Deque v (PrimStateEff) a ->
  Eff es [a]
dequeToList = D.dequeToList
{-# INLINE dequeToList #-}

-- | Lifted 'D.dequeToVector'.
dequeToVector ::
  (VG.Vector v' a, V.MVector v a, Prim :> es) =>
  Deque v (PrimStateEff) a ->
  Eff es (v' a)
dequeToVector = D.dequeToVector

-- | Lifted 'D.freezeDeque'.
freezeDeque ::
  (VG.Vector v a, Prim :> es) =>
  Deque (VG.Mutable v) (PrimStateEff) a ->
  Eff es (v a)
freezeDeque = D.freezeDeque
