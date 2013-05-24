module PFDS34 where

import LeftistHeap

weightMerge :: Ord a => Heap a -> Heap a -> Heap a
weightMerge Empty heap = heap
weightMerge heap Empty = heap
weightMerge left@(Heap _ lval ll lr) right@(Heap _ rval rl rr)
    | lval < rval   = makeT lval ll $ weightMerge lr right
    | otherwise     = makeT rval rl $ weightMerge left rr
  where
    rank :: Heap a -> Int
    rank Empty = 0
    rank (Heap r _ _ _) = r

    makeT val ltree rtree 
        | rank ltree >= rank rtree  = Heap (rank rtree + rank ltree + 1) val ltree rtree
        | otherwise                 = Heap (rank rtree + rank ltree + 1) val rtree ltree

weightInsert :: Ord a => a -> Heap a -> Heap a
weightInsert val = weightMerge (Heap 1 val Empty Empty)

weightDeleteMin :: Ord a => Heap a -> Heap a
weightDeleteMin Empty = error "heap underflow"
weightDeleteMin (Heap _ _ left right) = weightMerge left right
