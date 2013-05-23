module PFDS33 where

import LeftistHeap

fromList :: Ord a => [a] -> Heap a
fromList elems = go $ map (\v -> Heap 1 v Empty Empty) elems
    where
    step :: Ord a => [Heap a] -> [Heap a]
    step (h1:h2:hs) = (merge h1 h2) : step hs
    step h = h

    go :: Ord a => [Heap a] -> Heap a
    go [] = Empty
    go [s] = s
    go hs = go $ step hs
