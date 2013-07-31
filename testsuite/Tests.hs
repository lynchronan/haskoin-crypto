module Tests (tests) where

import Test.QuickCheck.Property
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Data.Maybe

import QuickCheckUtils

import Point
import Ring
import NumberTheory

tests = 
    [ testGroup "Number Theory" 
        [ testProperty "a * inv(a) = 1 (mod p)" inverseMod
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


