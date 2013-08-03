module Hash
( doubleSHA256
) where

import Crypto.Hash.SHA256 (hash)
import Data.Binary.Get (runGet)
import Data.Binary (get)

import qualified Data.ByteString as BS (ByteString)

import Ring (Hash256)
import Util (toLazyBS)

doubleSHA256 :: BS.ByteString -> Hash256
doubleSHA256 bs = runGet get (toLazyBS . hash . hash $ bs)

