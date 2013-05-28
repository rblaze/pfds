{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module LeftistHeap where

import BasicHeap

data LeftHeap a = Empty
                | Node !Int !a !(LeftHeap a) !(LeftHeap a)

newtype LHeap a = LHeap (LeftHeap a)

instance Ord a => Heap (LHeap a) a where
    empty = LHeap Empty
    insert val (LHeap heap) = LHeap $ insert' val heap
    findMin (LHeap heap) = findMin' heap
    deleteMin (LHeap heap) = LHeap $ deleteMin' heap
    toList (LHeap heap) = toList' heap

findMin' :: LeftHeap a -> Maybe a
findMin' Empty = Nothing
findMin' (Node _ val _ _) = Just val

deleteMin' :: Ord a => LeftHeap a -> LeftHeap a
deleteMin' Empty = error "heap underflow"
deleteMin' (Node _ _ left right) = merge left right

insert' :: Ord a => a -> LeftHeap a -> LeftHeap a
insert' val = merge (Node 1 val Empty Empty)

toList' :: LeftHeap a -> [a]
toList' Empty = []
toList' (Node _ val left right) = val : toList' left ++ toList' right

merge :: Ord a => LeftHeap a -> LeftHeap a -> LeftHeap a
merge Empty heap = heap
merge heap Empty = heap
merge left@(Node _ lval ll lr) right@(Node _ rval rl rr)
    | lval < rval   = makeT lval ll $ merge lr right
    | otherwise     = makeT rval rl $ merge left rr
  where
    rank :: LeftHeap a -> Int
    rank Empty = 0
    rank (Node r _ _ _) = r

    makeT val ltree rtree
        | rank ltree >= rank rtree  = Node (rank rtree + 1) val ltree rtree
        | otherwise                 = Node (rank ltree + 1) val rtree ltree
