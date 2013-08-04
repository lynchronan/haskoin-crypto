module Tests (tests) where

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

tests :: [Test]
tests = 
    [ testGroup "Number Theory" 
        [ testProperty "a * inv(a) = 1 (mod p)" inverseMod
        , testProperty "a * inv(a) = 1 (mod p) in FieldP" inverseModP
        , testProperty "a * inv(a) = 1 (mod n) in FieldN" inverseModN
        ],
      testGroup "Ring Numeric"
        [ testProperty "Ring fromInteger" ringFromInteger
        , testProperty "Ring addition" ringAddition
        , testProperty "Ring multiplication" ringMult
        , testProperty "Ring negation" ringNegate
        , testProperty "Ring abs" ringAbs
        , testProperty "Ring signum" ringSignum
        ],
      testGroup "Ring Bits"
        [ testProperty "Ring AND" ringAnd
        , testProperty "Ring OR" ringOr
        , testProperty "Ring XOR" ringXor
        , testProperty "Ring Complement" ringComplement
        , testProperty "Ring Shift" ringShift
        , testProperty "Ring Bitsize" ringBitsize
        , testProperty "Ring Testbit" ringTestbit
        , testProperty "Ring Bit" ringBit
        , testProperty "Ring PopCount" ringPopCount
        , testProperty "Ring IsSigned" ringIsSigned
        ],
      testGroup "Binary serialization"
        [ testProperty "get( put(Hash256) ) = Hash256" getPutHash256
        , testProperty "get( put(Hash160) ) = Hash160" getPutHash160
        , testProperty "get( put(FieldN) ) = FieldN" getPutModN
        , testProperty "Verify DER of put(FieldN)" putModNSize
        , testProperty "get( put(Sig) ) == Sig" getPutSig
        , testProperty "Verify DER of put(Sig)" putSigSize
        ],
      testGroup "Elliptic curve point arithmetic"
        [ testProperty "P is on the curve" checkOnCurve
        , testProperty "P1 + P2 is on the curve" addOnCurve
        , testProperty "n*P is on the curve" mulOnCurve
        , testProperty "makePoint (getAffine P) = P" fromToAffine
        , testProperty "P + InfPoint = P" addInfPoint
        , testProperty "InfPoint + P = P" addInfPoint'
        , testProperty "P1 + P2 = P2 + P1" addCommutative
        , testProperty "(P1 + P2) + P3 = P1 + (P2 + P3)" addAssoc
        , testProperty "(x,y) + (x,-y) = InfPoint" addInverseY
        , testProperty "double P = P + P" doubleAddPoint
        , testProperty "double P = 2*P" doubleMulPoint
        , testProperty "n*P = P + (n-1)*P" mulPointInduction
        , testProperty "a*P + b*P = (a + b)*P" mulDistributivity
        , testProperty "shamirsTrick = n1*P1 + n2*P2" testShamirsTrick
        ],
      testGroup "ECDSA signatures"
        [ testProperty "verify( sign(msg) ) = True" signAndVerify
        , testProperty "Signatures in ECDSA monad are unique" uniqueSignatures
        ]
    ]

{- Number Theory -}

inverseMod :: Word64 -> Property
inverseMod i = i > 0 ==> (i' * (mulInverse i' curveP)) `mod` curveP == 1
    where i' = fromIntegral i

inverseModP :: FieldP -> Property
inverseModP r = r > 0 ==> r/r == 1

inverseModN :: FieldN -> Property
inverseModN r = r > 0 ==> r/r == 1

{- Ring Numeric -}

ringFromInteger :: Integer -> Bool
ringFromInteger i = runRing ring == fromIntegral model
    where model = fromInteger i :: Word32
          ring  = fromInteger i :: Test32

ringAddition :: Integer -> Integer -> Bool
ringAddition i1 i2 = runRing ring == fromIntegral model
    where model = (fromInteger i1) + (fromInteger i2) :: Word32
          ring  = (fromInteger i1) + (fromInteger i2) :: Test32

ringMult :: Integer -> Integer -> Bool
ringMult i1 i2 = runRing ring == fromIntegral model
    where model = (fromInteger i1) * (fromInteger i2) :: Word32
          ring  = (fromInteger i1) * (fromInteger i2) :: Test32

ringNegate :: Integer -> Bool
ringNegate i = runRing ring == fromIntegral model
    where model = negate (fromInteger i) :: Word32
          ring  = negate (fromInteger i) :: Test32

ringAbs :: Integer -> Bool
ringAbs i = runRing ring == fromIntegral model
    where model = abs (fromInteger i) :: Word32
          ring  = abs (fromInteger i) :: Test32

ringSignum :: Integer -> Bool
ringSignum i = runRing ring == fromIntegral model
    where model = signum (fromInteger i) :: Word32
          ring  = signum (fromInteger i) :: Test32

{- Ring Bits -}

ringAnd :: Integer -> Integer -> Bool
ringAnd i1 i2 = runRing ring == fromIntegral model
    where model = (fromInteger i1) .&. (fromInteger i2) :: Word32
          ring  = (fromInteger i1) .&. (fromInteger i2) :: Test32

ringOr :: Integer -> Integer -> Bool
ringOr i1 i2 = runRing ring == fromIntegral model
    where model = (fromInteger i1) .|. (fromInteger i2) :: Word32
          ring  = (fromInteger i1) .|. (fromInteger i2) :: Test32
          
ringXor :: Integer -> Integer -> Bool
ringXor i1 i2 = runRing ring == fromIntegral model
    where model = (fromInteger i1) `xor` (fromInteger i2) :: Word32
          ring  = (fromInteger i1) `xor` (fromInteger i2) :: Test32

ringComplement :: Integer -> Bool
ringComplement i = runRing ring == fromIntegral model
    where model = complement (fromInteger i) :: Word32
          ring  = complement (fromInteger i) :: Test32

ringShift :: Integer -> Word8 -> Bool
ringShift i j = runRing ring == fromIntegral model
    where model = shift (fromInteger i) (fromIntegral j) :: Word32
          ring  = shift (fromInteger i) (fromIntegral j) :: Test32

ringBitsize :: Integer -> Bool
ringBitsize i = ring == model
    where model = bitSize ((fromInteger i) :: Word32) 
          ring  = bitSize ((fromInteger i) :: Test32) 

ringTestbit :: Integer -> Word8 -> Bool
ringTestbit i j = ring == model
    where model = testBit ((fromInteger i) :: Word32) (fromIntegral j)
          ring  = testBit ((fromInteger i) :: Test32) (fromIntegral j)

ringBit :: Word8 -> Bool
ringBit i = runRing ring == fromIntegral model
    where model = bit (fromIntegral i) :: Word32
          ring  = bit (fromIntegral i) :: Test32

ringPopCount :: Integer -> Bool
ringPopCount i = ring == model
    where model = popCount ((fromInteger i) :: Word32)
          ring  = popCount ((fromInteger i) :: Test32)

ringIsSigned :: Integer -> Bool
ringIsSigned i = ring == model
    where model = isSigned ((fromInteger i) :: Word32)
          ring  = isSigned ((fromInteger i) :: Test32)

{- Ring serialization -}

getPutHash256 :: Hash256 -> Bool
getPutHash256 r = r == runGet get (runPut $ put r)

getPutHash160 :: Hash160 -> Bool
getPutHash160 r = r == runGet get (runPut $ put r)

getPutModN :: FieldN -> Property
getPutModN r = r > 0 ==> r == runGet get (runPut $ put r)

getPutSig :: Signature -> Property
getPutSig sig@(Signature r s) = r > 0 && s > 0 ==> 
    sig == runGet get (runPut $ put sig)

putModNSize :: FieldN -> Property
putModNSize r = r > 0 ==>
    (  a == fromIntegral 0x02    -- DER type is Integer
    && b <= fromIntegral 33      -- Can't be bigger than 32 + 0x00 padding
    && l == fromIntegral (b + 2) -- Advertised length matches
    && c <= fromIntegral 0x7f    -- High byte is never 1
    )
    where bs = toStrictBS $ runPut $ put r
          a  = BS.index bs 0
          b  = BS.index bs 1
          c  = BS.index bs 2
          l  = BS.length bs

putSigSize :: Signature -> Property
putSigSize sig@(Signature r s) = r > 0 && s > 0 ==>
   (  a == fromIntegral 0x30    -- DER type is Sequence
   && b <= fromIntegral 70      -- Maximum length is 35 + 35
   && l == fromIntegral (b + 2) -- Advertised length matches
   )
   where bs = toStrictBS $ runPut $ put sig
         a  = BS.index bs 0
         b  = BS.index bs 1
         l  = BS.length bs

{- Public Key -}

checkOnCurve :: Point -> Bool
checkOnCurve InfPoint = True
checkOnCurve p = validatePoint p

addOnCurve :: Point -> Point -> Bool
addOnCurve p1 p2 = case addPoint p1 p2 of
    InfPoint -> True
    p        -> validatePoint p

mulOnCurve :: Point -> FieldN -> Bool
mulOnCurve p1 n = case mulPoint n p1 of
    InfPoint -> True
    p        -> validatePoint p

fromToAffine :: Point -> Property
fromToAffine p = not (isInfPoint p) ==> (fromJust $ makePoint x y) == p
    where (x,y) = fromJust $ getAffine p

addInfPoint :: Point -> Bool
addInfPoint p = addPoint p makeInfPoint == p

addInfPoint' :: Point -> Bool
addInfPoint' p = addPoint makeInfPoint p == p

addCommutative :: Point -> Point -> Bool
addCommutative p1 p2 = addPoint p1 p2 == addPoint p2 p1

addAssoc :: Point -> Point -> Point -> Bool
addAssoc p1 p2 p3 = 
    addPoint (addPoint p1 p2) p3 == addPoint p1 (addPoint p2 p3)

addInverseY :: Point -> Bool
addInverseY p1 = case (getAffine p1) of
    (Just (x,y)) -> addPoint p1 (fromJust $ makePoint x (-y)) == makeInfPoint
    Nothing      -> True

doubleAddPoint :: Point -> Bool
doubleAddPoint p = doublePoint p == addPoint p p

doubleMulPoint :: Point -> Bool
doubleMulPoint p = doublePoint p == mulPoint 2 p

mulPointInduction :: FieldN -> Point -> Property
mulPointInduction i p = i > 2 ==> 
    mulPoint i p == addPoint p (mulPoint (i-1) p)

mulDistributivity :: FieldN -> FieldN -> Point -> Bool
mulDistributivity a b p = 
    (addPoint (mulPoint a p) (mulPoint b p)) == mulPoint (a + b) p

testShamirsTrick :: FieldN -> Point -> FieldN -> Point -> Bool
testShamirsTrick n1 p1 n2 p2 = shamirRes == normalRes
    where shamirRes = shamirsTrick n1 p1 n2 p2
          normalRes = addPoint (mulPoint n1 p1) (mulPoint n2 p2)  

{- ECDSA Signatures -}

signAndVerify :: Hash256 -> PrivateKey -> Integer -> Property
signAndVerify msg d k = d > 0 ==> verifyMessage msg s q
    where q = mulPoint d curveG
          s = runIdentity $ withECDSA k (signMessage msg d)
           
uniqueSignatures :: Hash256 -> PrivateKey -> Integer -> Property
uniqueSignatures msg d k = d > 0 ==> r /= r' && s /= s'
    where ((r,s),(r',s')) = runIdentity $ withECDSA k $ do
            (Signature a b) <- signMessage msg d
            (Signature c d) <- signMessage msg d
            return ((a,b),(c,d))


