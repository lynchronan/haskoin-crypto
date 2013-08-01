module Hash
( doubleSHA256
, toStrictBS
, toLazyBS
) where

import Crypto.Hash.SHA256
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS

import Ring

doubleSHA256 :: BS.ByteString -> Hash256
doubleSHA256 bs = runGet B.get (toLazyBS . hash . hash $ bs)

toStrictBS :: BL.ByteString -> BS.ByteString
toStrictBS = BS.concat . BL.toChunks

toLazyBS :: BS.ByteString -> BL.ByteString
toLazyBS bs = BL.fromChunks [bs]

