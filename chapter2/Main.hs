module Main where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@?=), Assertion)

import Data.List (tails)

import PFDS21
import PFDS2Set
import PFDS22

tests :: [Test]
tests = [
    testGroup "suffixes" [
        testProperty "works as tails" prop_tails,
        testCase "book sample" testSample21
      ],
    testGroup "basic set" [
        testProperty "all members found" (prop_setMembersFound member),
        testProperty "some members found" (prop_setSomeMembersFound member),
        testProperty "no members in empty set" (prop_setEmpty member)
      ],
    testGroup "optimized member" [
        testProperty "all members found" (prop_setMembersFound member22),
        testProperty "some members found" (prop_setSomeMembersFound member22),
        testProperty "no members in empty set" (prop_setEmpty member22)
      ]
  ]

main :: IO ()
main = defaultMain tests

prop_tails :: [Int] -> Bool
prop_tails xs = suffixes xs == tails xs

testSample21 :: Assertion
testSample21 = suffixes ([1,2,3,4] :: [Int]) @?= [[1,2,3,4],[2,3,4],[3,4],[4],[]]

prop_setMembersFound :: (Int -> Tree Int -> Bool) -> [Int] -> Bool
prop_setMembersFound check xs = all (`check` set) xs
    where set = foldr insert Empty xs

prop_setSomeMembersFound :: (Int -> Tree Int -> Bool) -> [Int] -> Bool
prop_setSomeMembersFound check xs = all (\x -> check (x + 1) set == elem (x + 1) xs) xs
    where set = foldr insert Empty xs

prop_setEmpty :: (Int -> Tree Int -> Bool) -> [Int] -> Bool
prop_setEmpty check = all (\x -> not (check x Empty))
