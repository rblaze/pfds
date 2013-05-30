{-# LANGUAGE ScopedTypeVariables #-}
 
module Main where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Data.List(foldl')

import qualified Queue as Q

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
    testProperty "queue keeps all elements" prop_inout,
    testProperty "queue keeps all elements in complex case" prop_inout3
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
