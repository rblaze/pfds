module Main where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@?=), Assertion)

import Data.List (tails)

import PFDS21
import PFDS2Set

tests :: [Test]
tests = [
    testGroup "suffixes" [
        testProperty "works as tails" prop_tails,
        testCase "book sample" testSample21
      ],
    testGroup "basic set" [
        testProperty "all members found" prop_setMembersFound,
        testProperty "some members found" prop_setSomeMembersFound,
        testProperty "no members in empty set" prop_setEmpty
      ]
  ]

main :: IO ()
main = defaultMain tests

prop_tails :: [Int] -> Bool
prop_tails xs = suffixes xs == tails xs

prop_setMembersFound :: [Int] -> Bool
prop_setMembersFound xs = all (`member` set) xs
    where set = foldr insert Empty xs

prop_setSomeMembersFound :: [Int] -> Bool
prop_setSomeMembersFound xs = all (\x -> member (x + 1) set == elem (x + 1) xs) xs
    where set = foldr insert Empty xs

prop_setEmpty :: [Int] -> Bool
prop_setEmpty = all (\x -> not (member x Empty))

testSample21 :: Assertion
testSample21 = suffixes ([1,2,3,4] :: [Int]) @?= [[1,2,3,4],[2,3,4],[3,4],[4],[]]
