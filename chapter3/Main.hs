module Main where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.List(sort)

import LeftistHeap
import PFDS33

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
    heapTests "basic heap" (foldr insert Empty),
    heapTests "fromList" fromList
  ]

heapTests :: String -> ([Int] -> Heap Int) -> Test
heapTests name insertFunc = testGroup name [
    testProperty "heap contains all elements" (prop_allElemsPresent insertFunc),
    testProperty "heap elements sorted" (prop_heapElementsSorted insertFunc)
  ]

prop_allElemsPresent :: ([Int] -> Heap Int) -> [Int] -> Bool
prop_allElemsPresent mkheap xs = sort xs == sort (toList heap)
    where
    heap = mkheap xs

prop_heapElementsSorted :: ([Int] -> Heap Int) -> [Int] -> Bool
prop_heapElementsSorted mkheap xs = sort xs == unroll heap
    where
    heap = mkheap xs
    unroll h = case findMin h of
                Nothing -> []
                Just v  -> v : unroll (deleteMin h)
