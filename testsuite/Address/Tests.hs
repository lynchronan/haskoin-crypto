module Address.Tests (tests) where

import Test.QuickCheck.Property hiding ((.&.))
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Control.Monad.Identity
import Data.Maybe
import Data.Word
import Data.Bits
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS

import QuickCheckUtils

import ECDSA
import Point
import Ring
import NumberTheory
import Util
import Address

tests :: [Test]
tests = 
    [ testGroup "Address and Base58"
        [ testProperty "decode58( encode58(i) ) = i" decodeEncode58
        , testProperty "decode58Chk( encode58Chk(i) ) = i" decodeEncode58Check
        , testProperty "fromWIF( toWIF(i) ) = i" decodeEncodeWIF
        ]
    ]

decodeEncode58 :: BS.ByteString -> Bool
decodeEncode58 bs = case decodeBase58 (encodeBase58 bs) of
    (Just r) -> r == bs
    Nothing  -> False

decodeEncode58Check :: BS.ByteString -> Bool
decodeEncode58Check bs = case decodeBase58Check (encodeBase58Check bs) of
    (Just r) -> r == bs
    Nothing  -> False

decodeEncodeWIF :: FieldN -> Property
decodeEncodeWIF i = i > 0 ==> i == (fromJust $ wifToPrivkey $ privkeyToWIF i)

