{-# LANGUAGE ScopedTypeVariables #-}
 
module Main where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary)
import Data.List (foldl', sort)
import Data.Proxy

import BasicHeap
import qualified Queue as Q
import qualified Deque as D
import qualified SplayHeap as S
import qualified PairingHeap as P
import qualified BinaryPairingHeap as B

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
      ],
    heapTests "splay heap" (Proxy :: Proxy (S.SplayHeap Int)) [
        testProperty "toSortedList returns sorted elements" (prop_splayToSortedListIsSorted :: [Int] -> Bool)
      ],
    heapTests "pairing heap" (Proxy :: Proxy (P.PairingHeap Int)) [],
    heapTests "binary pairing heap" (Proxy :: Proxy (B.BinaryPairingHeap Int)) [
        testProperty "converted heap contains all elements" (prop_convertedAllElementsPresent :: [Int] -> Bool),
        testProperty "converted heap elements sorted" (prop_convertedHeapElementsSorted :: [Int] -> Bool)
      ]
  ]

heapTests :: (Arbitrary a, Show a, Ord a, Heap h a) => String -> Proxy h -> [Test] -> Test
heapTests name heaptype extra = testGroup name ([
    testProperty "heap contains all elements" (prop_allElementsPresent heaptype),
    testProperty "heap elements sorted" (prop_heapElementsSorted heaptype)
  ] ++ extra)

prop_allElementsPresent :: (Ord a, Heap h a) => Proxy h -> [a] -> Bool
prop_allElementsPresent hp xs = sort xs == sort (toList heap)
    where heap = fromList xs `asProxyTypeOf` hp

prop_heapElementsSorted :: (Ord a, Heap h a) => Proxy h -> [a] -> Bool
prop_heapElementsSorted hp xs = sort xs == unroll heap
    where
    heap = fromList xs `asProxyTypeOf` hp
    unroll h = case findMin h of
                Nothing -> []
                Just v  -> v : unroll (deleteMin h)

prop_splayToSortedListIsSorted :: Ord a => [a] -> Bool
prop_splayToSortedListIsSorted xs = sort xs == S.toSortedList heap
    where
    heap = fromList xs

prop_convertedAllElementsPresent :: Ord a => [a] -> Bool
prop_convertedAllElementsPresent xs = sort xs == sort (toList heap)
    where heap = B.convert $ fromList xs

prop_convertedHeapElementsSorted :: Ord a => [a] -> Bool
prop_convertedHeapElementsSorted xs = sort xs == unroll heap
    where
    heap = B.convert $ fromList xs
    unroll h = case findMin h of
                Nothing -> []
                Just v  -> v : unroll (deleteMin h)

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
