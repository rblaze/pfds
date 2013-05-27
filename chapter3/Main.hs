{-# LANGUAGE ScopedTypeVariables #-}
 
module Main where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck(Arbitrary)

import Data.List(sort)

import LeftistHeap
import PFDS33
import PFDS34

import BasicHeap
--import BinominalHeap

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
    heapTests "basic heap" (undefined :: LHeap Int),
    heapTests "better fromList" (undefined :: LHeap33 Int),
    heapTests "weight tree" (undefined :: LHeap34 Int)
  ]

heapTests :: (Arbitrary a, Show a, Ord a, Heap h) => String -> h a -> Test
heapTests name heaptype = testGroup name [
    testProperty "heap contains all elements" (prop_allElementsPresent heaptype),
    testProperty "heap elements sorted" (prop_heapElementsSorted heaptype)
  ]

prop_allElementsPresent :: (Ord a, Heap h) => h a -> [a] -> Bool
prop_allElementsPresent hp xs = sort xs == sort (toList heap)
    where heap = (fromList xs) `asTypeOf` hp

prop_heapElementsSorted :: (Ord a, Heap h) => h a -> [a] -> Bool
prop_heapElementsSorted hp xs = sort xs == unroll heap
    where
    heap = (fromList xs) `asTypeOf` hp
    unroll h = case findMin h of
                Nothing -> []
                Just v  -> v : unroll (deleteMin h)
