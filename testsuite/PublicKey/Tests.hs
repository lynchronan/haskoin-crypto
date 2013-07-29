module PublicKey.Tests (tests) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import QuickCheckUtils

import PublicKey

tests = 
    [ testGroup "PublicKey"
        [ testProperty "P + InfPoint = P" (t1 :: Point -> Bool)
        , testProperty "InfPoint + P = P" (t1' :: Point -> Bool)
        ]
    ]

t1 :: Point -> Bool
t1 p = addPoint p InfPoint == p

t1' :: Point -> Bool
t1' p = addPoint InfPoint p == p

