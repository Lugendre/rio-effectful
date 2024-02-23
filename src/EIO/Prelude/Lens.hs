module EIO.Prelude.Lens (
  view,
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
import Effectful.Reader.Static (Reader, asks)
import RIO hiding (Reader, asks, preview, view)

view :: (Reader s :> es) => Getting a s a -> Eff es a
view l = asks (getConst #. l Const)

preview :: (Reader s :> es) => Getting (First a) s a -> Eff es (Maybe a)
preview l = asks (getFirst #. foldMapOf l (First #. Just))