module Hash
( hash256
, doubleHash256
, chksum32
) where

import Data.Word (Word32)
import Crypto.Hash.SHA256 (hash)
import Data.Binary.Get (runGet)
import Data.Binary (get)
import Data.Bits (shiftR)

import qualified Data.ByteString as BS (ByteString)

import Ring (Hash256)
import Util (toLazyBS)

hash256 :: BS.ByteString -> Hash256
hash256 bs = runGet get (toLazyBS $ hash bs)

doubleHash256 :: BS.ByteString -> Hash256
doubleHash256 bs = runGet get (toLazyBS $ hash $ hash bs)

chksum32 :: BS.ByteString -> Word32
chksum32 bs = fromIntegral $ (doubleHash256 bs) `shiftR` 224

