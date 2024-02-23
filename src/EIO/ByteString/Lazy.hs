{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module EIO.ByteString.Lazy (
  module RIO.ByteString.Lazy,
  module EIO.ByteString.Lazy,
)
where

import Data.ByteString.Lazy qualified as BL
import Effectful (Eff, type (:>))
import Effectful.Console.ByteString (Console)
import Effectful.Console.ByteString.Lazy qualified as EBL
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.Path.IO.ByteString.Lazy qualified as EBL
import Path (File, Path)
import RIO (LByteString)
import RIO.ByteString.Lazy hiding (
  appendFile,
  getContents,
  hGet,
  hGetContents,
  hGetNonBlocking,
  hPut,
  hPutNonBlocking,
  hPutStr,
  interact,
  putStr,
  putStrLn,
  readFile,
  writeFile,
 )
import System.IO (Handle)

-- | Lifted 'BL.getContents'
getContents :: (Console :> es) => Eff es LByteString
getContents = EBL.getContents

-- | Lifted 'BL.putStr'
putStr :: (Console :> es) => LByteString -> Eff es ()
putStr = EBL.putStr

-- | Lifted 'BL.putStrLn'
putStrLn :: (Console :> es) => LByteString -> Eff es ()
putStrLn = EBL.putStrLn

-- | Lifted 'BL.interact'
interact :: (Console :> es) => (LByteString -> LByteString) -> Eff es ()
interact = EBL.interact

-- | Lifted 'BL.readFile'
readFile :: (FileSystem :> es) => Path b File -> Eff es LByteString
readFile = EBL.readFile

-- | Lifted 'BL.writeFile'
writeFile :: (FileSystem :> es) => Path b File -> LByteString -> Eff es ()
writeFile = EBL.writeFile

-- | Lifted 'BL.appendFile'
appendFile :: (FileSystem :> es) => Path b File -> LByteString -> Eff es ()
appendFile = EBL.appendFile

-- | Lifted 'BL.hGet'
hGet :: (Console :> es) => Handle -> Int -> Eff es LByteString
hGet h = unsafeEff_ . BL.hGet h

-- | Lifted 'BL.hGetContents'
hGetContents :: (Console :> es) => Handle -> Eff es LByteString
hGetContents = unsafeEff_ . BL.hGetContents

-- | Lifted 'BL.hGetNonBlocking'
hGetNonBlocking :: (Console :> es) => Handle -> Int -> Eff es LByteString
hGetNonBlocking h = unsafeEff_ . BL.hGetNonBlocking h

-- | Lifted 'BL.hPut'
hPut :: (Console :> es) => Handle -> LByteString -> Eff es ()
hPut h = unsafeEff_ . BL.hPut h

-- | Lifted 'BL.hPutNonBlocking'
hPutNonBlocking :: (Console :> es) => Handle -> LByteString -> Eff es LByteString
hPutNonBlocking h = unsafeEff_ . BL.hPutNonBlocking h

-- | Lifted 'BL.hPutStr'
hPutStr :: (Console :> es) => Handle -> LByteString -> Eff es ()
hPutStr h = unsafeEff_ . BL.hPutStr h