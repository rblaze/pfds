{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module RBTree where

import BasicTree

data Color = Red | Black
    deriving (Eq, Show)
data RBTree a = Empty
              | Node !Color !(RBTree a) a !(RBTree a)
  deriving Show

newtype RedBlackTree a = RedBlackTree (RBTree a)
  deriving Show

instance Ord a => Tree (RedBlackTree a) a where
    empty = RedBlackTree Empty
    member v (RedBlackTree tree) = member' v tree
    insert v (RedBlackTree tree) = RedBlackTree $ insert' v tree

member' :: (Ord a) => a -> RBTree a -> Bool
member' _ Empty = False
member' v (Node _ left val right)
    | v < val   = member' v left
    | v > val   = member' v right
    | otherwise = True

insert' :: (Ord a) => a -> RBTree a -> RBTree a
insert' v tree = Node Black l rv r
    where
    ins Empty = Node Red Empty v Empty
    ins n@(Node c left val right)
        | v < val   = balance c (ins left) val right
        | v > val   = balance c left val (ins right)
        | otherwise = n
    Node _ l rv r = ins tree

balance :: (Ord a) => Color -> RBTree a -> a -> RBTree a -> RBTree a
balance Black (Node Red (Node Red a x b) y c) z d = Node Red (Node Black a x b) y (Node Black c z d)
balance Black (Node Red a x (Node Red b y c)) z d = Node Red (Node Black a x b) y (Node Black c z d)
balance Black a x (Node Red (Node Red b y c) z d) = Node Red (Node Black a x b) y (Node Black c z d)
balance Black a x (Node Red b y (Node Red c z d)) = Node Red (Node Black a x b) y (Node Black c z d)
balance c left v right = Node c left v right
