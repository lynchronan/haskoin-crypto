module Util
( toStrictBS
, toLazyBS
, isolate
, integerToBS
, bsToInteger
) where

import Data.Word (Word8)
import Data.Binary.Get
    ( Get
    , runGetOrFail
    , getByteString
    )
import qualified Data.ByteString.Lazy as BL 
    ( ByteString
    , toChunks
    , fromChunks
    , length
    )
import qualified Data.ByteString as BS 
    ( ByteString
    , concat, length
    , pack, unpack
    )
import Data.Bits ((.|.), shiftL, shiftR)
import Data.List (unfoldr)

toStrictBS :: BL.ByteString -> BS.ByteString
toStrictBS = BS.concat . BL.toChunks

toLazyBS :: BS.ByteString -> BL.ByteString
toLazyBS bs = BL.fromChunks [bs]

bsToInteger :: BS.ByteString -> Integer
bsToInteger = (foldr f 0) . reverse . BS.unpack
    where f w n = (toInteger w) .|. shiftL n 8

integerToBS :: Integer -> BS.ByteString
integerToBS 0 = BS.pack [0]
integerToBS i 
    | i > 0    = BS.pack $ reverse $ unfoldr f i
    | otherwise = error "integerToBS not defined for negative values"
    where f 0 = Nothing
          f x = Just $ (fromInteger x :: Word8, x `shiftR` 8)

-- Isolate a Get monad for the next Int bytes
-- Fails if the input monad failed or some input was not consumed
isolate :: Int -> Get a -> Get a
isolate i g = do
    bs <- getByteString i
    case runGetOrFail g (toLazyBS bs) of
        (Left (_, _, err)) -> fail err
        (Right (bs, _, res))
            | BL.length bs > 0 -> fail "Isolate: unconsumed input"
            | otherwise -> return res

