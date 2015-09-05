{-# LANGUAGE ScopedTypeVariables #-}
 
module Main where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck(Arbitrary)

import Data.Proxy

import BasicTree
import qualified RBTree as RB

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
    treeTests "basic tree" (Proxy :: Proxy (RB.RBTree Int))
  ]

treeTests :: forall t a . (Arbitrary a, Show a, Ord a, Num a, Tree t a) => String -> Proxy t -> Test
treeTests name treetype = testGroup name [
    testProperty "tree balanced" (prop_treeBalanced :: [a] -> Bool),
    testProperty "tree contains all elements" (prop_allElementsPresentInTree treetype),
    testProperty "tree not contains missing elements" (prop_noneElementsPresentInTree treetype),
    testProperty "tree misses deleted elements" (prop_deletedElementsNotMembers treetype)
  ]

prop_allElementsPresentInTree :: (Ord a, Tree t a) => Proxy t -> [a] -> Bool
prop_allElementsPresentInTree treetype xs = all (`member` tree) xs
    where
    tree = foldr insert empty xs `asProxyTypeOf` treetype

prop_noneElementsPresentInTree :: (Num a, Ord a, Tree t a) => Proxy t -> [a] -> Bool
prop_noneElementsPresentInTree treetype xs = not $ any (`member` tree) $ map (+ delta) xs
    where
    delta = 1 + maximum xs - minimum xs
    tree = foldr insert empty xs `asProxyTypeOf` treetype

prop_treeBalanced :: Ord a => [a] -> Bool
prop_treeBalanced xs = verifyColors tree && verifyDepth tree
    where
    tree = RB.treeData $ foldr insert empty xs

verifyColors :: RB.RBTreeData a -> Bool
verifyColors RB.Empty = True
verifyColors (RB.Node _ c left _ right) = checkChilds && verifyColors left && verifyColors right
    where
    checkChilds = case c of
                    RB.Black -> True
                    RB.Red   -> (color left == RB.Black) && (color right == RB.Black)
    color RB.Empty = RB.Black
    color (RB.Node _ col _ _ _) = col

verifyDepth :: RB.RBTreeData a -> Bool
verifyDepth tree = checkDepth 0 tree
    where
    depth = getDepth 0 tree
    addDepth :: Int -> RB.Color -> Int
    addDepth n color = if color == RB.Black then n + 1 else n
    getDepth n RB.Empty = n
    getDepth n (RB.Node _ color left _ _) = getDepth (addDepth n color) left
    checkDepth n RB.Empty = n == depth
    checkDepth n (RB.Node _ color left _ right) = let d = addDepth n color
                                                   in checkDepth d left && checkDepth d right

prop_deletedElementsNotMembers :: (Show a, Num a, Ord a, Tree t a) => Proxy t -> [a] -> Bool
prop_deletedElementsNotMembers treetype xs = not (any (`member` tree) ys) && all (`member` tree) zs'
    where
    (ys, zs) = splitAt ((length xs `div` 2) + 1) xs
    zs' = filter (`notElem` ys) zs
    basetree = foldr insert empty xs `asProxyTypeOf` treetype
    tree = foldr delete basetree ys
