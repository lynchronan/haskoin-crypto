module Haskoin.Crypto.Keys.Tests (tests) where

import Test.QuickCheck.Property hiding ((.&.))
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Data.Maybe
import Data.Word
import Data.Bits
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS

import QuickCheckUtils

import Haskoin.Crypto.Keys
import Haskoin.Crypto.Point
import Haskoin.Crypto.Util

tests :: [Test]
tests = 
    [ testGroup "PublicKey Binary"
        [ testProperty "get( put(PublicKey) ) = PublicKey" getPutPoint
        , testProperty "size( put(Point) ) = 33 or 65" putPointSize
        ],
      testGroup "Key formats"
        [ testProperty "fromWIF( toWIF(i) ) = i" fromToWIF
        ]
    ]

{- Public Key Binary -}

getPutPoint :: PublicKey -> Bool
getPutPoint p = p == runGet get (runPut $ put p)

putPointSize :: PublicKey -> Bool
putPointSize p = case p of
    (PublicKey  InfPoint) -> BS.length s == 1
    (PublicKey  _)        -> BS.length s == 33
    (PublicKeyU InfPoint) -> BS.length s == 1
    (PublicKeyU _)        -> BS.length s == 65
    where s = toStrictBS $ runPut $ put p

{- Key formats -}

fromToWIF :: PrivateKey -> Property
fromToWIF pk = i > 0 ==> pk == (fromJust $ fromWIF $ toWIF pk)
    where i = runPrivateKey pk



