module Tests (tests) where

import Test.QuickCheck.Property
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Data.Maybe

import QuickCheckUtils
import Secp256k1
import PublicKey
import PrivateKey
import NumberTheory

tests = 
    [ testGroup "PublicKey"
        [ testProperty "P is on the curve" checkOnCurve
        , testProperty "P1 + P2 is on the curve" addOnCurve
        , testProperty "n*P is on the curve" mulOnCurve
        , testProperty "makePoint (getAffine P) = P" fromToAffine
        , testProperty "P + InfPoint = P" addInfPoint
        , testProperty "InfPoint + P = P" addInfPoint'
        , testProperty "P1 + P2 = P2 + P1" addCommutative
        , testProperty "(x,y) + (x,-y) = InfPoint" addInverseY
        , testProperty "double P = P + P" doubleAddPoint
        , testProperty "double P = 2*P" doubleMulPoint
        , testProperty "n*P = P + (n-1)*P" mulPointInduction
        ],
      testGroup "Number Theory" 
        [ testProperty "a * inv(a) = 1 (mod p)" inverseMod
        ]
    ]

{- Number Theory -}

inverseMod :: Integer -> Property
inverseMod i = i > 0 ==> (i * (mulInverse i p)) `mod` p == 1

{- Public Key -}

checkOnCurve :: Point -> Bool
checkOnCurve = checkPoint

addOnCurve :: Point -> Point -> Bool
addOnCurve p1 p2 = checkPoint $ addPoint p1 p2

mulOnCurve :: Point -> Fn -> Bool
mulOnCurve p1 n = checkPoint $ mulPoint p1 n

fromToAffine :: Point -> Property
fromToAffine p = not (isInfPoint p) ==> (fromJust $ makePoint x y) == p
    where (x,y) = fromJust $ getAffine p

addInfPoint :: Point -> Bool
addInfPoint p = addPoint p InfPoint == p

addInfPoint' :: Point -> Bool
addInfPoint' p = addPoint InfPoint p == p

addCommutative :: Point -> Point -> Bool
addCommutative p1 p2 = addPoint p1 p2 == addPoint p2 p1

addInverseY :: Point -> Bool
addInverseY p1 = case (getAffine p1) of
    (Just (x,y)) -> addPoint p1 (fromJust $ makePoint x (-y)) == InfPoint
    Nothing      -> True

doubleAddPoint :: Point -> Bool
doubleAddPoint p = doublePoint p == addPoint p p

doubleMulPoint :: Point -> Bool
doubleMulPoint p = doublePoint p == mulPoint p 2

mulPointInduction :: Point -> Fn -> Property
mulPointInduction p i = (runFn i) > 2 ==> 
    mulPoint p i == addPoint p (mulPoint p (i-1))

