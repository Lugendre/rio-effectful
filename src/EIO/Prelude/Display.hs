module EIO.Prelude.Display (
  Utf8Builder (..),
  Display (..),
  displayShow,
  utf8BuilderToText,
  utf8BuilderToLazyText,
  displayBytesUtf8,
  module EIO.Prelude.Display,
) where

import RIO hiding (writeFileUtf8Builder)
import RIO qualified

writeFileUtf8Builder :: (MonadIO m) => FilePath -> Utf8Builder -> m ()
writeFileUtf8Builder = RIO.writeFileUtf8Builder
