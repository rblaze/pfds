{-# LANGUAGE ScopedTypeVariables #-}
 
module Main where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck(Arbitrary)
import Data.List(foldl')

import qualified Queue as Q

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
    testProperty "queue keeps all elements" prop_inout
  ]

prop_inout :: [Int] -> Bool
prop_inout xs = xs == popAll queue
    where
    queue = foldl' Q.push Q.empty xs
    popAll q | Q.null q = []
    popAll q = Q.head q : popAll (Q.pop q)
