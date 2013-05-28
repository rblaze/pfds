{-# LANGUAGE ScopedTypeVariables #-}
 
module Main where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck(Arbitrary)

import Data.List(sort)

import BasicHeap

import LeftistHeap
import PFDS33
import PFDS34

import qualified BinominalHeap
import qualified PFDS36
import qualified PFDS37

import qualified RBTree as RB

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
    heapTests "basic heap" (undefined :: LHeap Int),
    heapTests "better fromList" (undefined :: LHeap33 Int),
    heapTests "weight heap" (undefined :: LHeap34 Int),
    heapTests "binominal heap" (undefined :: BinominalHeap.BinomHeap Int),
    heapTests "binominal heap w/o rank" (undefined :: PFDS36.BinomHeap Int),
    heapTests "cached heap" (undefined :: PFDS37.CachedHeap (PFDS36.BinomHeap Int) Int),
    testProperty "tree balanced" prop_treeBalanced,
    testProperty "tree contains all elements" prop_allElementsPresentInTree,
    testProperty "tree not contains missing elements" prop_noneElementsPresentInTree
  ]

heapTests :: (Arbitrary a, Show a, Ord a, Heap h a) => String -> h -> Test
heapTests name heaptype = testGroup name [
    testProperty "heap contains all elements" (prop_allElementsPresent heaptype),
    testProperty "heap elements sorted" (prop_heapElementsSorted heaptype)
  ]

prop_allElementsPresent :: (Ord a, Heap h a) => h -> [a] -> Bool
prop_allElementsPresent hp xs = sort xs == sort (toList heap)
    where heap = fromList xs `asTypeOf` hp

prop_heapElementsSorted :: (Ord a, Heap h a) => h -> [a] -> Bool
prop_heapElementsSorted hp xs = sort xs == unroll heap
    where
    heap = fromList xs `asTypeOf` hp
    unroll h = case findMin h of
                Nothing -> []
                Just v  -> v : unroll (deleteMin h)

prop_treeBalanced :: [Int] -> Bool
prop_treeBalanced xs = verifyColors $ mktree xs
    where
    mktree = foldr RB.insert RB.Empty

prop_allElementsPresentInTree :: [Int] -> Bool
prop_allElementsPresentInTree xs = all (`RB.member` tree) xs
    where
    tree = foldr RB.insert RB.Empty xs

prop_noneElementsPresentInTree :: [Int] -> Bool
prop_noneElementsPresentInTree xs = not $ any (`RB.member` tree) $ map (\x -> x * 2 + 1) xs
    where
    tree = foldr (RB.insert . (* 2)) RB.Empty xs

verifyColors :: RB.Tree a -> Bool
verifyColors RB.Empty = True
verifyColors (RB.Node c left _ right) = checkChilds && verifyColors left && verifyColors right
    where
    checkChilds = case c of
                    RB.Black -> True
                    RB.Red   -> (color left == RB.Black) && (color right == RB.Black)
    color RB.Empty = RB.Black
    color (RB.Node col _ _ _) = col
