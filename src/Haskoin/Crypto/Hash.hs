module Haskoin.Crypto.Hash
( Hash256
, Hash160
, CheckSum32
, hash256
, hash160
, hash256BS
, hash160BS
, doubleHash256
, doubleHash256BS
, chksum32
) where

import Data.Word (Word32)
import Crypto.Hash 
    ( Digest 
    , SHA256
    , RIPEMD160
    , hash
    , digestToByteString
    )
import Data.Binary.Get (runGet)
import Data.Binary (Binary, get, put)
import Data.Bits (shiftR)
import Control.Applicative ((<$>))

import qualified Data.ByteString as BS (ByteString)

import Haskoin.Crypto.Ring (Hash256, Hash160)
import Haskoin.Crypto.Util (toLazyBS)

newtype CheckSum32 = CheckSum32 { runCheckSum32 :: Word32 }
    deriving (Show, Eq)

instance Binary CheckSum32 where
    get = CheckSum32 <$> get
    put (CheckSum32 w) = put w

run256 :: BS.ByteString -> BS.ByteString
run256 = (digestToByteString :: Digest SHA256 -> BS.ByteString) . hash

run160 :: BS.ByteString -> BS.ByteString
run160 = (digestToByteString :: Digest RIPEMD160 -> BS.ByteString) . hash

hash256 :: BS.ByteString -> Hash256
hash256 bs = runGet get (toLazyBS $ run256 bs)

hash256BS :: BS.ByteString -> BS.ByteString
hash256BS bs = run256 bs

hash160 :: BS.ByteString -> Hash160
hash160 bs = runGet get (toLazyBS $ run160 bs)

hash160BS :: BS.ByteString -> BS.ByteString
hash160BS bs = run160 bs

doubleHash256 :: BS.ByteString -> Hash256
doubleHash256 bs = runGet get (toLazyBS $ run256 $ run256 bs)

doubleHash256BS :: BS.ByteString -> BS.ByteString
doubleHash256BS bs = run256 $ run256 bs

chksum32 :: BS.ByteString -> CheckSum32
chksum32 bs = CheckSum32 $ fromIntegral $ (doubleHash256 bs) `shiftR` 224

