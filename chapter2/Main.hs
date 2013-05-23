module Main where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@?=), Assertion)
import Test.QuickCheck (Arbitrary(..), choose)

import Data.List (tails, group, sort, nubBy)
import Data.Function
import Data.Maybe
import Control.Monad

import PFDS21
import PFDS2Set
import PFDS22
import PFDS23
import PFDS24
import PFDS25
import qualified PFDS26 as M

newtype TreeDepth = TreeDepth Int
    deriving Show

newtype NodeCount = NodeCount Int
    deriving Show

instance Arbitrary TreeDepth where
    arbitrary = liftM TreeDepth $ choose (1, 20)

instance Arbitrary NodeCount where
    arbitrary = liftM NodeCount $ choose (0, 10000)

tests :: [Test]
tests = [
    testGroup "suffixes" [
        testProperty "works as tails" prop_tails,
        testCase "book sample" testSample21
      ],
    setTests "basic set" member insert,
    setTests "optimized member" member22 insert,
    setTests "optimized insert" member insert23,
    setTests "overoptimized insert" member insert24,
    testGroup "collapsed trees" [
        testProperty "nelems in complete tree" prop_depth
      ],
    testGroup "almost balanced trees" [
        testProperty "nelems in tree" prop_count,
        testProperty "balance" prop_balance
      ],
    testGroup "finite map" [
        testCase "fixed case" testFixedMap
      ]
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

testFixedMap :: Assertion
testFixedMap = (present && nonpresent) @?= True
    where
    elements = [(6,'a'),(8,'b'),(4,'c'),(2,'d'),(10,'e'),(2,'f'),(2,'g'),(2,'h'),
                (2,'i'),(2,'j'),(0,'k'),(14,'l'),(2,'m'),(2,'n'),(2,'o'),(12,'p')]
    uniqs = nubBy ((==) `on` fst) elements
    set = foldr (\(k,v) t -> M.bind k v t) M.Empty elements
    present = all (\(k,v) -> M.lookup k set == Just v) uniqs
    nonpresent = not $ any (\k -> isJust (M.lookup k set)) ((-1) : [n + 1 | (n, _) <- elements])

prop_tails :: [Int] -> Bool
prop_tails xs = suffixes xs == tails xs

prop_depth :: TreeDepth -> Bool
prop_depth (TreeDepth depth) = length (toList set) == (2 ^ depth) - 1
    where set = mktree25a 'а' depth

prop_count :: NodeCount -> Bool
prop_count (NodeCount cnt) = length (toList set) == cnt
    where set = mktree25b 'b' cnt

prop_balance :: NodeCount -> Bool
prop_balance (NodeCount cnt) = go set
    where
    set = mktree25b 'b' cnt
    go :: Tree a -> Bool
    go Empty = True
    go (Tree left _ right) = 
        go left && go right &&
        abs (length (toList left) - length (toList right)) <= 1

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
prop_allElemsPresent ins xs = uniq == sort (toList set)
    where
    set = foldr ins Empty xs
    uniq = map head $ group $ sort xs

prop_setMembersFound :: (Int -> Tree Int -> Bool) -> (Int -> Tree Int -> Tree Int) -> [Int] -> Bool
prop_setMembersFound check ins xs = all (`check` set) xs
    where set = foldr ins Empty xs

prop_setSomeMembersFound :: (Int -> Tree Int -> Bool) -> (Int -> Tree Int -> Tree Int) -> [Int] -> Bool
prop_setSomeMembersFound check ins xs
    = all (\x -> check (x + 1) set == elem (x + 1) xs) xs
    where set = foldr ins Empty xs

prop_setEmpty :: (Int -> Tree Int -> Bool) -> [Int] -> Bool
prop_setEmpty check xs = not $ any (`check` Empty) xs
