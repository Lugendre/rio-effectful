{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module EIO.ByteString (
  module Effectful.FileSystem.Path.IO.ByteString,
  module Effectful.Console.ByteString,
  module RIO.ByteString,
  module EIO.ByteString,
) where

import Effectful (Eff, type (:>))
import Effectful.Console.ByteString
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.FileSystem.Path.IO.ByteString
import RIO.ByteString hiding (
  appendFile,
  fromFilePath,
  getContents,
  getLine,
  hGet,
  hGetContents,
  hGetLine,
  hGetNonBlocking,
  hGetSome,
  hPut,
  hPutNonBlocking,
  hPutStr,
  interact,
  putStr,
  readFile,
  toFilePath,
  writeFile,
 )
import RIO.ByteString qualified as B
import System.IO (Handle)

-- | Lifted 'B.hGetLine'
hGetLine :: (Console :> es) => Handle -> Eff es ByteString
hGetLine = unsafeEff_ . B.hGetLine

-- | Lifted 'B.hGetContents'
hGetContents :: (Console :> es) => Handle -> Eff es ByteString
hGetContents = unsafeEff_ . B.hGetContents

-- | Lifted 'B.hGet'
hGet :: (Console :> es) => Handle -> Int -> Eff es ByteString
hGet h = unsafeEff_ . B.hGet h

-- | Lifted 'B.hGetSome'
hGetSome :: (Console :> es) => Handle -> Int -> Eff es ByteString
hGetSome h = unsafeEff_ . B.hGetSome h

-- | Lifted 'B.hGetNonBlocking'
hGetNonBlocking :: (Console :> es) => Handle -> Int -> Eff es ByteString
hGetNonBlocking h = unsafeEff_ . B.hGetNonBlocking h

-- | Lifted 'B.hPut'
hPut :: (Console :> es) => Handle -> ByteString -> Eff es ()
hPut h = unsafeEff_ . B.hPut h

-- | Lifted 'B.hPutNonBlocking'
hPutNonBlocking :: (Console :> es) => Handle -> ByteString -> Eff es ByteString
hPutNonBlocking h = unsafeEff_ . B.hPutNonBlocking h

-- | Lifted 'B.hPutStr'
hPutStr :: (Console :> es) => Handle -> ByteString -> Eff es ()
hPutStr h = unsafeEff_ . B.hPutStr h