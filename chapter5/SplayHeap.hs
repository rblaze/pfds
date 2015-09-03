{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module SplayHeap where

import BasicHeap

data SplayHeap a = Empty
                 | Node !(SplayHeap a) !a !(SplayHeap a)

instance Ord a => Heap (SplayHeap a) a where
    empty = Empty
    insert = insert'
    toList = toList'
    findMin = findMin'
    deleteMin = deleteMin'

insert' :: Ord a => a -> SplayHeap a -> SplayHeap a
insert' a t = Node (smaller a t) a (bigger a t)

bigger :: Ord a => a -> SplayHeap a -> SplayHeap a
bigger _ Empty = Empty
bigger p (Node _ v right)
    | v <= p = bigger p right
bigger _ n@(Node Empty _ _) = n
bigger p (Node (Node left' v' right') v right)
    | v' <= p = Node (bigger p right') v right
    | otherwise = Node (bigger p left') v' (Node right' v right)

smaller :: Ord a => a -> SplayHeap a -> SplayHeap a
smaller _ Empty = Empty
smaller p (Node left v _)
    | v > p = smaller p left
smaller _ n@(Node _ _ Empty) = n
smaller p (Node left v (Node left' v' right'))
    | v' > p = Node left v (smaller p left')
    | otherwise = Node (Node left v left') v' (smaller p right')

toList' :: SplayHeap a -> [a]
toList' Empty = []
toList' (Node left v right) = v : toList' left ++ toList' right

findMin' :: SplayHeap a -> Maybe a
findMin' Empty = Nothing
findMin' (Node Empty v _) = Just v
findMin' (Node n _ _) = findMin' n

deleteMin' :: SplayHeap a -> SplayHeap a
deleteMin' Empty = error "deletion from empty heap"
deleteMin' (Node Empty _ right) = right
deleteMin' (Node (Node Empty _ right') v right) = Node right' v right
deleteMin' (Node (Node left' v' right') v right) = Node (deleteMin' left') v' (Node right' v right)

toSortedList :: SplayHeap a -> [a]
toSortedList Empty = []
toSortedList (Node left v right) = toSortedList left ++ v : toSortedList right
