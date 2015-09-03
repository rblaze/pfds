{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module BinaryPairingHeap where

import BasicHeap
import qualified PairingHeap as P

data BinaryPairingHeap a = Empty
                         | Node (BinaryPairingHeap a) a (BinaryPairingHeap a)

instance Ord a => Heap (BinaryPairingHeap a) a where
    empty = Empty
    insert = insert'
    toList = toList'
    findMin = findMin'
    deleteMin = deleteMin'

toList' :: BinaryPairingHeap a -> [a]
toList' Empty = []
toList' (Node left v right) = toList' left ++ v : toList' right

merge :: Ord a => BinaryPairingHeap a -> BinaryPairingHeap a -> BinaryPairingHeap a
merge Empty Empty = Empty
merge h Empty = h
merge Empty h = h
merge (Node l1 v1 Empty) (Node l2 v2 Empty)
    | v1 < v2 = Node (Node l2 v2 l1) v1 Empty
    | otherwise = Node (Node l1 v1 l2) v2 Empty
merge _ _ = error "merging root with right child"

insert' :: Ord a => a -> BinaryPairingHeap a -> BinaryPairingHeap a
insert' v h = merge h (Node Empty v Empty)

findMin' :: BinaryPairingHeap a -> Maybe a
findMin' Empty = Nothing
findMin' (Node _ v _) = Just v

deleteMin' :: Ord a => BinaryPairingHeap a -> BinaryPairingHeap a
deleteMin' Empty = error "delete from empty heap"
deleteMin' (Node left _ Empty) = go left
    where
    go Empty = Empty
    go n@(Node _ _ Empty) = n
    go (Node left1 v1 (Node left2 v2 right2)) = merge (merge (Node left1 v1 Empty) (Node left2 v2 Empty)) (go right2)
deleteMin' _ = error "delete from root with right child"

convert :: P.PairingHeap a -> BinaryPairingHeap a
convert P.Empty = Empty
convert (P.Node v []) = Node Empty v Empty
convert (P.Node v xs) = Node (foldr step Empty xs) v Empty
    where
    step n t = let Node left v' Empty = convert n
                in Node left v' t
