module Main where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.List(sort)

import BaseHeap

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
    heapTests "basic heap" insert
  ]

heapTests :: String -> (Int -> Heap Int -> Heap Int) -> Test
heapTests name insertFunc = testGroup name [
    testProperty "heap contains all elements" (prop_allElemsPresent insertFunc),
    testProperty "heap elements sorted" (prop_heapElementsSorted insertFunc)
  ]

prop_allElemsPresent :: (Int -> Heap Int -> Heap Int) -> [Int] -> Bool
prop_allElemsPresent ins xs = sort xs == sort (toList heap)
    where
    heap = foldr ins Empty xs

prop_heapElementsSorted :: (Int -> Heap Int -> Heap Int) -> [Int] -> Bool
prop_heapElementsSorted ins xs = sort xs == unroll heap
    where
    heap = foldr ins Empty xs
    unroll h = case findMin h of
                Nothing -> []
                Just v  -> v : unroll (deleteMin h)
