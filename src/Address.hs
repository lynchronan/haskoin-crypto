module Address where

import Data.Char (ord)
import Data.Word (Word8)
import Data.Maybe (fromJust)
import Data.Bits (shiftL)
import Control.Applicative ((<$>))
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M

import Hash (chksum32)
import Util (integerToBS)

b58Str :: String
b58Str = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

b58 :: BS.ByteString
b58 = BS.pack $ map (fromIntegral . ord) b58Str

b58Map :: M.Map Word8 Int
b58Map = M.fromList $ zip (BS.unpack b58) [0..57]

encodeBase58 :: Integer -> BS.ByteString
encodeBase58 0 = BS.pack [49] -- ord '1' = 49
encodeBase58 i
    | i >= 0 = go BS.empty i
    | otherwise = error "encodeBase58 is not defined for negative Integers"
    where go acc 0 = acc
          go acc n = go (BS.cons (fromIntegral b) acc) q
              where (q,r) = n `quotRem` 58
                    b     = BS.index b58 (fromIntegral r)

-- We return maybe Integer because the ByteString could contain invalid
-- characters like 0,O,l,I
decodeBase58 :: BS.ByteString -> Maybe Integer
decodeBase58 b = go 0 b
    where go i bs 
            | BS.null bs = Just i
            | otherwise  = do
                n <- fromIntegral <$> M.lookup (BS.head bs) b58Map
                go (i*58 + n) (BS.tail bs)
          
encodeBase58Check :: Integer -> BS.ByteString
encodeBase58Check i = encodeBase58 $ (i `shiftL` 32) + chk
    where bs  = integerToBS i
          chk = toInteger $ chksum32 bs

