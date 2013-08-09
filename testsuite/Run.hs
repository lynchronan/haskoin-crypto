module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import qualified Ring.Tests (tests)
import qualified Point.Tests (tests)
import qualified ECDSA.Tests (tests)
import qualified Address.Tests (tests)
import qualified Units (tests)

main = defaultMain 
    (  Ring.Tests.tests 
    ++ Point.Tests.tests 
    ++ ECDSA.Tests.tests 
    ++ Address.Tests.tests 
    ++ Units.tests
    )

