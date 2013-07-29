module PublicKey.Tests (tests) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import QuickCheckUtils

import PublicKey

tests = 
    [ testGroup "PublicKey"
        [ testProperty "P is on the curve" checkOnCurve
        , testProperty "P + InfPoint = P" addInfPoint
        , testProperty "InfPoint + P = P" addInfPoint'
        , testProperty "P1 + P2 = P2 + P1" addCommutative
        ]
    ]

checkOnCurve :: Point -> Bool
checkOnCurve = checkPoint

addInfPoint :: Point -> Bool
addInfPoint p = addPoint p InfPoint == p

addInfPoint' :: Point -> Bool
addInfPoint' p = addPoint InfPoint p == p

addCommutative :: Point -> Point -> Bool
addCommutative p1 p2 = addPoint p1 p2 == addPoint p2 p1

