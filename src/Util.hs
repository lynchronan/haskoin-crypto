module Util
( toStrictBS
, toLazyBS
) where

import qualified Data.ByteString.Lazy as BL 
    ( ByteString
    , toChunks
    , fromChunks
    )
import qualified Data.ByteString as BS 
    ( ByteString
    , concat
    )

toStrictBS :: BL.ByteString -> BS.ByteString
toStrictBS = BS.concat . BL.toChunks

toLazyBS :: BS.ByteString -> BL.ByteString
toLazyBS bs = BL.fromChunks [bs]

