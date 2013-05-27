module PFDS33 where

import BasicHeap
import LeftistHeap

newtype LHeap33 a = LHeap33 (LeftHeap a)

instance Heap LHeap33 where
    empty = LHeap33 Empty
    insert val (LHeap33 heap) = LHeap33 $ insert' val heap
    findMin (LHeap33 heap) = findMin' heap
    deleteMin (LHeap33 heap) = LHeap33 $ deleteMin' heap
    toList (LHeap33 heap) = toList' heap
    fromList xs = LHeap33 $ fromList33 xs

fromList33 :: Ord a => [a] -> LeftHeap a
fromList33 elems = go $ map (\v -> Node 1 v Empty Empty) elems
    where
    step :: Ord a => [LeftHeap a] -> [LeftHeap a]
    step (h1:h2:hs) = merge h1 h2 : step hs
    step h = h

    go :: Ord a => [LeftHeap a] -> LeftHeap a
    go [] = Empty
    go [s] = s
    go hs = go $ step hs
