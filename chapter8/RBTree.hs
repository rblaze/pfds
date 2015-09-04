{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module RBTree where

import BasicTree

data Color = Red | Black
    deriving (Eq, Show)
data RBTree a = Empty
              | Node !Bool !Color !(RBTree a) a !(RBTree a)
  deriving Show

instance Ord a => Tree (RBTree a) a where
    empty = Empty
    member = member'
    insert = insert'
    delete = delete'

member' :: Ord a => a -> RBTree a -> Bool
member' _ Empty = False
member' v (Node e _ left val right)
    | v < val   = member' v left
    | v > val   = member' v right
    | otherwise = e

insert' :: Ord a => a -> RBTree a -> RBTree a
insert' v tree = Node erv Black l rv r
    where
    ins Empty = Node True Red Empty v Empty
    ins (Node eval c left val right)
        | v < val   = balance eval c (ins left) val right
        | v > val   = balance eval c left val (ins right)
        | otherwise = Node True c left val right
    Node erv _ l rv r = ins tree

balance :: Ord a => Bool -> Color -> RBTree a -> a -> RBTree a -> RBTree a
balance ez Black (Node ey Red (Node ex Red a x b) y c) z d = Node ey Red (Node ex Black a x b) y (Node ez Black c z d)
balance ez Black (Node ex Red a x (Node ey Red b y c)) z d = Node ey Red (Node ex Black a x b) y (Node ez Black c z d)
balance ex Black a x (Node ez Red (Node ey Red b y c) z d) = Node ey Red (Node ex Black a x b) y (Node ez Black c z d)
balance ex Black a x (Node ey Red b y (Node ez Red c z d)) = Node ey Red (Node ex Black a x b) y (Node ez Black c z d)
balance ev c left v right = Node ev c left v right

delete' :: Ord a => a -> RBTree a -> RBTree a
delete' _ Empty = Empty
delete' v (Node e c left val right)
    | v < val = Node e c (delete' v left) val right
    | v > val = Node e c left val (delete' v right)
    | otherwise = Node False c left val right
