{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module EIO.Prelude.Display (
  Utf8Builder (..),
  Display (..),
  displayShow,
  utf8BuilderToText,
  utf8BuilderToLazyText,
  displayBytesUtf8,
  module EIO.Prelude.Display,
) where

import Effectful (Eff, type (:>))
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.FileSystem (FileSystem)
import Path (File, Path)
import Path qualified as P
import RIO hiding (writeFileUtf8Builder)
import RIO qualified

-- | Lifted 'RIO.writeFileUtf8Builder'.
writeFileUtf8Builder :: (FileSystem :> es) => Path b File -> Utf8Builder -> Eff es ()
writeFileUtf8Builder path = unsafeEff_ . RIO.writeFileUtf8Builder (P.toFilePath path)
