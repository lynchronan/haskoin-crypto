module Units (tests) where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import qualified Data.ByteString as BS

import Control.Monad.Identity
import Data.Maybe

import Haskoin.Crypto.Keys
import Haskoin.Crypto.Ring
import Haskoin.Crypto.Point
import Haskoin.Crypto.ECDSA
import Haskoin.Crypto.Hash
import Haskoin.Crypto.Util

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

sigMsg = [("Very secret message " ++ (show i) ++ ": 11") | i <- [0..15]]

sec1 = fromJust $ fromWIF strSecret1
sec2 = fromJust $ fromWIF strSecret2
sec1C = fromJust $ fromWIF strSecret1C
sec2C = fromJust $ fromWIF strSecret2C
pub1 = derivePublicKey sec1
pub2 = derivePublicKey sec2
pub1C = derivePublicKey sec1C
pub2C = derivePublicKey sec2C

tests =
    [ testGroup "bitcoind /src/test/key_tests.cpp" $
        [ testCase "Decode Valid WIF" checkPrivkey
        , testCase "Decode Invalid WIF" checkInvalidKey
        , testCase "Check private key compression" checkPrivateKeyCompressed
        , testCase "Check public key compression" checkKeyCompressed
        , testCase "Check matching address" checkMatchingAddress
        ] ++ 
        ( map (\x -> (testCase ("Check sig: " ++ (show x)) 
                (checkSignatures $ doubleHash256 $ stringToBS x))) sigMsg )
    ]

checkPrivkey = do
    assertBool "Key 1"  $ isJust $ fromWIF strSecret1
    assertBool "Key 2"  $ isJust $ fromWIF strSecret2
    assertBool "Key 1C" $ isJust $ fromWIF strSecret1C
    assertBool "Key 2C" $ isJust $ fromWIF strSecret2C

checkInvalidKey = 
    assertBool "Bad key" $ isNothing $ fromWIF strAddressBad

checkPrivateKeyCompressed = do
    assertBool "Key 1"  $ not $ isPrivateKeyCompressed sec1
    assertBool "Key 2"  $ not $ isPrivateKeyCompressed sec2
    assertBool "Key 1C" $ isPrivateKeyCompressed sec1C
    assertBool "Key 2C" $ isPrivateKeyCompressed sec2C

checkKeyCompressed = do
    assertBool "Key 1"  $ not $ isCompressed pub1
    assertBool "Key 2"  $ not $ isCompressed pub2
    assertBool "Key 1C" $ isCompressed pub1C
    assertBool "Key 2C" $ isCompressed pub2C

checkMatchingAddress = do
    assertBool "Key 1"  $ addr1  == publicKeyAddress pub1
    assertBool "Key 2"  $ addr2  == publicKeyAddress pub2
    assertBool "Key 1C" $ addr1C == publicKeyAddress pub1C
    assertBool "Key 2C" $ addr2C == publicKeyAddress pub2C
    
checkSignatures h = do
    assertBool "Key 1, Sign1"   $ verifyMessage h sign1 pub1
    assertBool "Key 1, Sign2"   $ not $ verifyMessage h sign2 pub1
    assertBool "Key 1, Sign1C"  $ verifyMessage h sign1C pub1
    assertBool "Key 1, Sign2C"  $ not $ verifyMessage h sign2C pub1
    assertBool "Key 2, Sign1"   $ not $ verifyMessage h sign1 pub2
    assertBool "Key 2, Sign2"   $ verifyMessage h sign2 pub2
    assertBool "Key 2, Sign1C"  $ not $ verifyMessage h sign1C pub2
    assertBool "Key 2, Sign2C"  $ verifyMessage h sign2C pub2
    assertBool "Key 1C, Sign1"  $ verifyMessage h sign1 pub1C
    assertBool "Key 1C, Sign2"  $ not $ verifyMessage h sign2 pub1C
    assertBool "Key 1C, Sign1C" $ verifyMessage h sign1C pub1C
    assertBool "Key 1C, Sign2C" $ not $ verifyMessage h sign2C pub1C
    assertBool "Key 2C, Sign1"  $ not $ verifyMessage h sign1 pub2C
    assertBool "Key 2C, Sign2"  $ verifyMessage h sign2 pub2C
    assertBool "Key 2C, Sign1C" $ not $ verifyMessage h sign1C pub2C
    assertBool "Key 2C, Sign2C" $ verifyMessage h sign2C pub2C
    where 
        (sign1, sign2, sign1C, sign2C) = runIdentity $ withECDSA 1 $ do
            a <- signMessage h sec1
            b <- signMessage h sec2
            c <- signMessage h sec1C
            d <- signMessage h sec2C
            return (a,b,c,d)


