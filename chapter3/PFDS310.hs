{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module PFDS310 where

import BasicTree
import RBTree

newtype FastTree a = FastTree (RBTree a)
  deriving Show

instance Ord a => Tree (FastTree a) a where
    empty = FastTree Empty
    member v (FastTree tree) = member' v tree
    insert v (FastTree tree) = FastTree $ insertFast v tree

insertFast :: (Ord a) => a -> RBTree a -> RBTree a
insertFast v tree = Node Black l rv r
    where
    ins Empty = Node Red Empty v Empty
    ins n@(Node c left val right)
        | v < val   = lbalance c (ins left) val right
        | v > val   = rbalance c left val (ins right)
        | otherwise = n
    Node _ l rv r = ins tree

rbalance :: (Ord a) => Color -> RBTree a -> a -> RBTree a -> RBTree a
rbalance Black a x (Node Red (Node Red b y c) z d) = Node Red (Node Black a x b) y (Node Black c z d)
rbalance Black a x (Node Red b y (Node Red c z d)) = Node Red (Node Black a x b) y (Node Black c z d)
rbalance c left v right = Node c left v right

lbalance :: (Ord a) => Color -> RBTree a -> a -> RBTree a -> RBTree a
lbalance Black (Node Red (Node Red a x b) y c) z d = Node Red (Node Black a x b) y (Node Black c z d)
lbalance Black (Node Red a x (Node Red b y c)) z d = Node Red (Node Black a x b) y (Node Black c z d)
lbalance c left v right = Node c left v right
