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

import BasicTree as BT
import qualified RBTree as RB

import qualified PFDS39

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
    treeTests "basic tree" unordCreate unpack,
    treeTests "fromOrdList" ordCreate unpack
  ]
    where
    unordCreate :: [Int] -> RB.RedBlackTree Int
    unordCreate = foldr BT.insert BT.empty
    unpack :: RB.RedBlackTree Int -> RB.RBTree Int
    unpack (RB.RedBlackTree tree) = tree
    ordCreate :: [Int] -> RB.RedBlackTree Int
    ordCreate xs = PFDS39.fromOrdList $ sort xs

heapTests :: (Arbitrary a, Show a, Ord a, Heap h a) => String -> h -> Test
heapTests name heaptype = testGroup name [
    testProperty "heap contains all elements" (prop_allElementsPresent heaptype),
    testProperty "heap elements sorted" (prop_heapElementsSorted heaptype)
  ]

treeTests :: (Arbitrary a, Show a, Ord a, Num a, BT.Tree t a) => String -> ([a] -> t) -> (t -> RB.RBTree a) -> Test
treeTests name create unpack = testGroup name [
    testProperty "tree balanced" (prop_treeBalanced (unpack . create)),
    testProperty "tree contains all elements" (prop_allElementsPresentInTree create),
    testProperty "tree not contains missing elements" (prop_noneElementsPresentInTree create)
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

prop_allElementsPresentInTree :: (Ord a, BT.Tree t a) => ([a] -> t) -> [a] -> Bool
prop_allElementsPresentInTree create xs = all (`BT.member` tree) xs
    where
    tree = create xs

prop_noneElementsPresentInTree :: (Num a, Ord a, BT.Tree t a) => ([a] -> t) -> [a] -> Bool
prop_noneElementsPresentInTree create xs = not $ any (`BT.member` tree) $ map (+ delta) xs
    where
    delta = 1 + maximum xs - minimum xs
    tree = create xs

prop_treeBalanced :: (Ord a) => ([a] -> RB.RBTree a) -> [a] -> Bool
prop_treeBalanced create xs = verifyColors tree && verifyDepth tree
    where
    tree = create xs

verifyColors :: RB.RBTree a -> Bool
verifyColors RB.Empty = True
verifyColors (RB.Node c left _ right) = checkChilds && verifyColors left && verifyColors right
    where
    checkChilds = case c of
                    RB.Black -> True
                    RB.Red   -> (color left == RB.Black) && (color right == RB.Black)
    color RB.Empty = RB.Black
    color (RB.Node col _ _ _) = col

verifyDepth :: RB.RBTree a -> Bool
verifyDepth tree = checkDepth 0 tree
    where
    depth = getDepth 0 tree
    addDepth :: Int -> RB.Color -> Int
    addDepth n color = if color == RB.Black then n + 1 else n
    getDepth n RB.Empty = n
    getDepth n (RB.Node color left _ _) = getDepth (addDepth n color) left
    checkDepth n RB.Empty = n == depth
    checkDepth n (RB.Node color left _ right) = let d = addDepth n color
                                                 in checkDepth d left && checkDepth d right
