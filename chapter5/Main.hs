{-# LANGUAGE ScopedTypeVariables #-}
 
module Main where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Data.List(foldl')

import qualified Queue as Q
import qualified Deque as D

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
    testGroup "queue" [
        testProperty "keeps all elements" prop_inout,
        testProperty "keeps all elements in complex case" prop_inout3
      ],
    testGroup "deque" [
        testProperty "keeps all elements" prop_deque_inout,
        testProperty "keeps all elements in complex case" prop_deque_inout3,
        testProperty "keeps all elements on reverse" prop_deque_rev_inout,
        testProperty "keeps all elements on reverse in complex case" prop_deque_rev_inout3
      ]
  ]

prop_inout :: [Int] -> Bool
prop_inout xs = xs == popAll queue
    where
    queue = foldl' Q.snoc Q.empty xs
    popAll q | Q.null q = []
    popAll q = Q.head q : popAll (Q.tail q)

prop_inout3 :: [Int] -> [Int] -> [Int] -> Bool
prop_inout3 xs ys zs = concat [xs, ys, zs] == popped && Q.null queue
    where
    queuex = foldl' Q.snoc Q.empty xs
    queuexy = foldl' Q.snoc queuex ys
    (queuey, poppedx) = popN (length xs) queuexy
    queueyz = foldl' Q.snoc queuey zs
    (queue, poppedyz) = popN (length ys + length zs) queueyz
    popped = poppedx ++ poppedyz
    
    popN 0 q = (q, [])
    popN n q = let (tq, l) = popN (n - 1) (Q.tail q) in (tq, Q.head q : l)

prop_deque_inout :: [Int] -> Bool
prop_deque_inout xs = xs == popAll queue
    where
    queue = foldl' D.snoc D.empty xs
    popAll q | D.null q = []
    popAll q = D.head q : popAll (D.tail q)

prop_deque_rev_inout :: [Int] -> Bool
prop_deque_rev_inout xs = xs == popAll queue
    where
    queue = foldl' (flip D.cons) D.empty xs
    popAll q | D.null q = []
    popAll q = D.last q : popAll (D.init q)

prop_deque_inout3 :: [Int] -> [Int] -> [Int] -> Bool
prop_deque_inout3 xs ys zs = concat [xs, ys, zs] == popped && D.null queue
    where
    queuex = foldl' D.snoc D.empty xs
    queuexy = foldl' D.snoc queuex ys
    (queuey, poppedx) = popN (length xs) queuexy
    queueyz = foldl' D.snoc queuey zs
    (queue, poppedyz) = popN (length ys + length zs) queueyz
    popped = poppedx ++ poppedyz
    
    popN 0 q = (q, [])
    popN n q = let (tq, l) = popN (n - 1) (D.tail q) in (tq, D.head q : l)

prop_deque_rev_inout3 :: [Int] -> [Int] -> [Int] -> Bool
prop_deque_rev_inout3 xs ys zs = concat [xs, ys, zs] == popped && D.null queue
    where
    queuex = foldl' (flip D.cons) D.empty xs
    queuexy = foldl' (flip D.cons) queuex ys
    (queuey, poppedx) = popN (length xs) queuexy
    queueyz = foldl' (flip D.cons) queuey zs
    (queue, poppedyz) = popN (length ys + length zs) queueyz
    popped = poppedx ++ poppedyz
    
    popN 0 q = (q, [])
    popN n q = let (tq, l) = popN (n - 1) (D.init q) in (tq, D.last q : l)
