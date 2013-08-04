module Util
( toStrictBS
, toLazyBS
, isolate
) where

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
    , concat
    , length
    )

toStrictBS :: BL.ByteString -> BS.ByteString
toStrictBS = BS.concat . BL.toChunks

toLazyBS :: BS.ByteString -> BL.ByteString
toLazyBS bs = BL.fromChunks [bs]

-- Isolate a Get monad for the next Int bytes
-- Fails in the input monad failed or some input was not consumed
isolate :: Int -> Get a -> Get a
isolate i g = do
    bs <- getByteString i
    case runGetOrFail g (toLazyBS bs) of
        (Left (_, _, err)) -> fail err
        (Right (bs, _, res))
            | BL.length bs > 0 -> fail "Isolate: unconsumed input"
            | otherwise -> return res

