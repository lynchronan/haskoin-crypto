module Hash
( doubleSHA256
, toStrictBS
, toLazyBS
) where

import Crypto.Hash.SHA256 (hash)
import Data.Binary.Get (runGet)
import Data.Binary (get)
import qualified Data.ByteString.Lazy as BL 
    ( ByteString
    , toChunks
    , fromChunks
    )
import qualified Data.ByteString as BS 
    ( ByteString
    , concat
    )

import Ring (Hash256)

doubleSHA256 :: BS.ByteString -> Hash256
doubleSHA256 bs = runGet get (toLazyBS . hash . hash $ bs)

toStrictBS :: BL.ByteString -> BS.ByteString
toStrictBS = BS.concat . BL.toChunks

toLazyBS :: BS.ByteString -> BL.ByteString
toLazyBS bs = BL.fromChunks [bs]

