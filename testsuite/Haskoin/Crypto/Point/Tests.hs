module Haskoin.Crypto.Point.Tests (tests) where

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

import Haskoin.Crypto.Point
import Haskoin.Crypto.Ring
import Haskoin.Crypto.NumberTheory
import Haskoin.Crypto.Util

tests :: [Test]
tests = 
    [ testGroup "Elliptic curve point arithmetic"
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
        ]
    ]

{- Elliptic curve point arithmetic -}

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

