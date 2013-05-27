module PFDS34 where

import LeftistHeap
import BasicHeap

newtype LHeap34 a = LHeap34 (LeftHeap a)

instance Heap LHeap34 where
    empty = LHeap34 Empty
    insert val (LHeap34 heap) = LHeap34 $ weightInsert val heap
    findMin (LHeap34 heap) = findMin' heap
    deleteMin (LHeap34 heap) = LHeap34 $ weightDeleteMin heap
    toList (LHeap34 heap) = toList' heap

weightMerge :: Ord a => LeftHeap a -> LeftHeap a -> LeftHeap a
weightMerge Empty heap = heap
weightMerge heap Empty = heap
weightMerge left@(Node _ lval ll lr) right@(Node _ rval rl rr)
    | lval < rval   = makeT lval ll $ weightMerge lr right
    | otherwise     = makeT rval rl $ weightMerge left rr
  where
    rank :: LeftHeap a -> Int
    rank Empty = 0
    rank (Node r _ _ _) = r

    makeT val ltree rtree 
        | rank ltree >= rank rtree  = Node (rank rtree + rank ltree + 1) val ltree rtree
        | otherwise                 = Node (rank rtree + rank ltree + 1) val rtree ltree

weightInsert :: Ord a => a -> LeftHeap a -> LeftHeap a
weightInsert val = weightMerge (Node 1 val Empty Empty)

weightDeleteMin :: Ord a => LeftHeap a -> LeftHeap a
weightDeleteMin Empty = error "heap underflow"
weightDeleteMin (Node _ _ left right) = weightMerge left right
