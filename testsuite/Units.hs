module Units (tests) where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

tests =
    [ testGroup "Unit Tests"
        [ testCase "some test" someTest
        ]
    ]

someTest = 1 @?= 2

