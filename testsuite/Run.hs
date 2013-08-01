module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.QuickCheck
import Test.HUnit

import qualified Tests (tests)
import qualified Units (tests)

main = defaultMain (Tests.tests ++ Units.tests)

