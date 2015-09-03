{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module PairingHeap where

import BasicHeap

data PairingHeap a = Empty
                   | Node a [PairingHeap a]

instance Ord a => Heap (PairingHeap a) a where
    empty = Empty
    insert = insert'
    toList = toList'
    findMin = findMin'
    deleteMin = deleteMin'

findMin' :: PairingHeap a -> Maybe a
findMin' Empty = Nothing
findMin' (Node v _) = Just v

merge :: Ord a => PairingHeap a -> PairingHeap a -> PairingHeap a
merge h1@(Node v1 xs1) h2@(Node v2 xs2)
    | v1 < v2 = Node v1 (h2:xs1)
    | otherwise = Node v2 (h1:xs2)
merge Empty Empty = Empty
merge Empty h = h
merge h Empty = h

insert' :: Ord a => a -> PairingHeap a -> PairingHeap a
insert' v h = merge h (Node v [])

toList' :: PairingHeap a -> [a]
toList' Empty = []
toList' (Node v xs) = v : concatMap toList' xs

deleteMin' :: Ord a => PairingHeap a -> PairingHeap a
deleteMin' Empty = error "delete from empty heap"
deleteMin' (Node _ xs) = go xs
    where
    go [] = Empty
    go [x] = x
    go (x:y:ys) = merge (merge x y) (go ys)
