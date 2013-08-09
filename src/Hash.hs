module Hash
( CheckSum32
, hash256
, doubleHash256
, chksum32
) where

import Data.Word (Word32)
import Crypto.Hash.SHA256 (hash)
import Data.Binary.Get (runGet)
import Data.Binary (Binary, get, put)
import Data.Bits (shiftR)
import Control.Applicative ((<$>))

import qualified Data.ByteString as BS (ByteString)

import Ring (Hash256)
import Util (toLazyBS)

newtype CheckSum32 = CheckSum32 { runCheckSum32 :: Word32 }

instance Binary CheckSum32 where
    get = CheckSum32 <$> get
    put (CheckSum32 w) = put w

hash256 :: BS.ByteString -> Hash256
hash256 bs = runGet get (toLazyBS $ hash bs)

doubleHash256 :: BS.ByteString -> Hash256
doubleHash256 bs = runGet get (toLazyBS $ hash $ hash bs)

chksum32 :: BS.ByteString -> CheckSum32
chksum32 bs = CheckSum32 $ fromIntegral $ (doubleHash256 bs) `shiftR` 224

