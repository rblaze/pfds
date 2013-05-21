module Main where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@?=), Assertion)

import Data.List (tails, group, sort)

import PFDS21
import PFDS2Set
import PFDS22
import PFDS23

tests :: [Test]
tests = [
    testGroup "suffixes" [
        testProperty "works as tails" prop_tails,
        testCase "book sample" testSample21
      ],
    setTests "basic set" member insert,
    setTests "optimized member" member22 insert,
    setTests "optimized insert" member insert23
  ]

setTests :: String -> (Int -> Tree Int -> Bool) -> (Int -> Tree Int -> Tree Int) -> Test
setTests name memberFunc insertFunc = testGroup name [
    testProperty "set contains all elements" (prop_allElemsPresent insertFunc),
    testProperty "all members found" (prop_setMembersFound memberFunc insertFunc),
    testProperty "some members found" (prop_setSomeMembersFound memberFunc insertFunc),
    testProperty "no members in empty set" (prop_setEmpty memberFunc),
    testCase "fixed case" (testFixedSet memberFunc insertFunc)
  ]

main :: IO ()
main = defaultMain tests

prop_tails :: [Int] -> Bool
prop_tails xs = suffixes xs == tails xs

testSample21 :: Assertion
testSample21 = suffixes ([1,2,3,4] :: [Int]) @?= [[1,2,3,4],[2,3,4],[3,4],[4],[]]

testFixedSet :: (Int -> Tree Int -> Bool) -> (Int -> Tree Int -> Tree Int) -> Assertion
testFixedSet check ins = (present && nonpresent) @?= True
    where
    numbers = [6,8,4,2,10,2,2,2,2,2,0,14,2,2,2,12]
    set = foldr ins Empty numbers
    present = all (`check` set) numbers
    nonpresent = not $ any (`check` set) ((-1) : [n + 1 | n <- numbers])

prop_allElemsPresent :: (Int -> Tree Int -> Tree Int) -> [Int] -> Bool
prop_allElemsPresent ins xs = uniq xs == sort (toList set)
    where
    set = foldr ins Empty xs
    uniq ys = map head $ group $ sort ys

prop_setMembersFound :: (Int -> Tree Int -> Bool) -> (Int -> Tree Int -> Tree Int) -> [Int] -> Bool
prop_setMembersFound check ins xs = all (`check` set) xs
    where set = foldr ins Empty xs

prop_setSomeMembersFound :: (Int -> Tree Int -> Bool) -> (Int -> Tree Int -> Tree Int) -> [Int] -> Bool
prop_setSomeMembersFound check ins xs
    = all (\x -> check (x + 1) set == elem (x + 1) xs) xs
    where set = foldr ins Empty xs

prop_setEmpty :: (Int -> Tree Int -> Bool) -> [Int] -> Bool
prop_setEmpty check xs = not $ any (`check` Empty) xs
