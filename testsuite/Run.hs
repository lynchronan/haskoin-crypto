module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Tests (tests)

main = defaultMain Tests.tests

