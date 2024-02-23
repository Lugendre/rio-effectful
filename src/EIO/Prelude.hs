module EIO.Prelude (
  module RIO.Prelude,
  module RIO.Prelude.Types,
)
where

import RIO.Prelude hiding (
  ask,
  asks,
  local,
  runReader,
  runReaderT,
 )
import RIO.Prelude.Types hiding (
  MonadReader,
  PrimMonad (PrimState),
  Reader,
  ReaderT (ReaderT),
 )
