module Address where

import Data.Char (ord)
import Data.Word (Word8)
import Data.Maybe (fromJust)
import Data.Bits (shiftL)
import Data.Binary (put)
import Data.Binary.Put (runPut)

import Control.Applicative ((<$>))
import Control.Monad (guard)

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M

import Hash (chksum32)
import Util (integerToBS, bsToInteger, toStrictBS)

b58String :: String
b58String = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

b58Data :: BS.ByteString
b58Data = BS.pack $ map (fromIntegral . ord) b58String

b58Data' :: M.Map Word8 Int
b58Data' = M.fromList $ zip (BS.unpack b58Data) [0..57]

b58 :: Word8 -> Word8
b58 i = BS.index b58Data (fromIntegral i)

b58' :: Word8 -> Maybe Word8
b58' w = fromIntegral <$> M.lookup w b58Data'

encodeBase58I :: Integer -> BS.ByteString
encodeBase58I 0 = BS.pack [b58 0]
encodeBase58I i
    | i >= 0 = go BS.empty i
    | otherwise = error "encodeBase58 is not defined for negative Integers"
    where go acc 0 = acc
          go acc n = go (BS.cons (fromIntegral b) acc) q
              where (q,r) = n `quotRem` 58
                    b     = b58 $ fromIntegral r

encodeBase58 :: BS.ByteString -> BS.ByteString
encodeBase58 bs = BS.append l r
    where (z,b) = BS.span (== 0) bs
          l     = BS.map b58 z -- preserve leading 0's
          r | BS.null b = BS.empty
            | otherwise = encodeBase58I $ bsToInteger b

-- We return maybe Integer because the ByteString could contain invalid
-- characters like 0,O,l,I
decodeBase58 :: BS.ByteString -> Maybe BS.ByteString
decodeBase58 bs = r >>= return . (BS.append prefix)
    where (z,b)  = BS.span (== (b58 0)) bs
          prefix = BS.map (fromJust . b58') z -- preserve leading 1's
          r | BS.null b = Just BS.empty
            | otherwise = integerToBS <$> foldl f (Just 0) (BS.unpack b)
          f i w  = do
              n <- fromIntegral <$> b58' w
              p <- i
              return $ p*58 + n

encodeBase58Check :: BS.ByteString -> BS.ByteString
encodeBase58Check bs = encodeBase58 $ BS.append bs chk
    where chk = toStrictBS $ runPut $ put (chksum32 bs)

-- decoding fails if the bytestring contains invalid base58 characters
-- or if the checksum doesn't match
decodeBase58Check :: BS.ByteString -> Maybe BS.ByteString
decodeBase58Check bs = do
    rs <- decodeBase58 bs
    let (res,chk) = BS.splitAt ((BS.length rs) - 4) rs
    guard $ chk == (toStrictBS $ runPut $ put (chksum32 res))
    return res

