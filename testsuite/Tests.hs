module Tests (tests) where

import Test.QuickCheck.Property hiding ((.&.))
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Data.Maybe
import Data.Word
import Data.Bits

import QuickCheckUtils

import Point
import Ring
import NumberTheory

tests = 
    [ testGroup "Number Theory" 
        [ testProperty "a * inv(a) = 1 (mod p)" inverseMod
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
        ]
    ]

{- Number Theory -}

inverseMod :: Integer -> Property
inverseMod i = i > 0 ==> (i * (mulInverse i curveP)) `mod` curveP == 1

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

{- Public Key -}

checkOnCurve :: Point -> Bool
checkOnCurve = checkPoint

addOnCurve :: Point -> Point -> Bool
addOnCurve p1 p2 = checkPoint $ addPoint p1 p2

mulOnCurve :: Point -> FieldN -> Bool
mulOnCurve p1 n = checkPoint $ mulPoint p1 n

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
doubleMulPoint p = doublePoint p == mulPoint p 2

mulPointInduction :: Point -> FieldN -> Property
mulPointInduction p i = i > 2 ==> 
    mulPoint p i == addPoint p (mulPoint p (i-1))

mulDistributivity :: FieldN -> FieldN -> Point -> Bool
mulDistributivity a b p = 
    (addPoint (mulPoint p a) (mulPoint p b)) == mulPoint p (a + b)


