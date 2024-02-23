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

import Data.Monoid (First)
import RIO hiding (preview, view)
import RIO qualified

view :: (MonadReader s m) => Getting a s a -> m a
view = RIO.view

preview :: (MonadReader s m) => Getting (First a) s a -> m (Maybe a)
preview = RIO.preview