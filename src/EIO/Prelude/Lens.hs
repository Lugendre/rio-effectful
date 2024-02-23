module EIO.Prelude.Lens (
  staticView,
  view,
  staticPreview,
  preview,
  ASetter,
  ASetter',
  Getting,
  Lens,
  Lens',
  SimpleGetter,
  lens,
  over,
  set,
  sets,
  to,
  (^.),
  (^?),
  (^..),
  (%~),
  (.~),
) where

import Control.Lens (foldMapOf)
import Data.Monoid (First (..))
import Data.Profunctor.Unsafe ((#.))
import Effectful (Eff, type (:>))
import Effectful.Reader.Dynamic qualified as Dynamic
import Effectful.Reader.Static qualified as Static
import RIO hiding (Reader, asks, preview, view)

staticView :: (Static.Reader s :> es) => Getting a s a -> Eff es a
staticView l = Static.asks (getConst #. l Const)

staticPreview :: (Static.Reader s :> es) => Getting (First a) s a -> Eff es (Maybe a)
staticPreview l = Static.asks (getFirst #. foldMapOf l (First #. Just))

view :: (Dynamic.Reader s :> es) => Getting a s a -> Eff es a
view l = Dynamic.asks (getConst #. l Const)

preview :: (Dynamic.Reader s :> es) => Getting (First a) s a -> Eff es (Maybe a)
preview l = Dynamic.asks (getFirst #. foldMapOf l (First #. Just))