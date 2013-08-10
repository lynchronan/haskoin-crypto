module Units (tests) where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import qualified Data.ByteString as BS

import Data.Maybe
import Data.Char

import Address
import Ring
import Point
import ECDSA
import Util

stringToBS :: String -> BS.ByteString
stringToBS s = BS.pack $ map (fromIntegral . ord) s

-- Unit tests copied from bitcoind implementation
-- https://github.com/bitcoin/bitcoin/blob/master/src/test/key_tests.cpp

strSecret1  = stringToBS "5HxWvvfubhXpYYpS3tJkw6fq9jE9j18THftkZjHHfmFiWtmAbrj"
strSecret2  = stringToBS "5KC4ejrDjv152FGwP386VD1i2NYc5KkfSMyv1nGy1VGDxGHqVY3"
strSecret1C = stringToBS "Kwr371tjA9u2rFSMZjTNun2PXXP3WPZu2afRHTcta6KxEUdm1vEw"
strSecret2C = stringToBS "L3Hq7a8FEQwJkW1M2GNKDW28546Vp5miewcCzSqUD9kCAXrJdS3g"

addr1 = stringToBS "1QFqqMUD55ZV3PJEJZtaKCsQmjLT6JkjvJ"
addr2 = stringToBS "1F5y5E5FMc5YzdJtB9hLaUe43GDxEKXENJ"
addr1C = stringToBS "1NoJrossxPBKfCHuJXT4HadJrXRE9Fxiqs"
addr2C = stringToBS "1CRj2HyM1CXWzHAXLQtiGLyggNT9WQqsDs"

strAddressBad = stringToBS "1HV9Lc3sNHZxwj4Zk6fB38tEmBryq2cBiF"

tests =
    [ testGroup "Private keys"
        [ testCase "Decoding WIF" checkPrivkey
        ]
    ]

checkPrivkey = do
    assertBool "Key 1" (isJust $ wifToPrivkey strSecret1)
    assertBool "Key 2" (isJust $ wifToPrivkey strSecret2)
    assertBool "Key 1C" (isJust $ wifToPrivkey strSecret1C)
    assertBool "Key 2C" (isJust $ wifToPrivkey strSecret2C)

