{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module RBTree where

import Data.Bits
import Data.List

import BasicTree

data Color = Red | Black
    deriving (Eq, Show)

data RBTreeData a = Empty
                  | Node !Bool !Color !(RBTreeData a) a !(RBTreeData a)
    deriving Show

data RBTree a = RBTree { nTotal :: !Word, nDeleted:: !Word, treeData :: RBTreeData a }
    deriving Show

instance Ord a => Tree (RBTree a) a where
    empty = RBTree 0 0 Empty
    member = member'
    insert = insert'
    delete = delete'

member' :: Ord a => a -> RBTree a -> Bool
member' v tree = go $ treeData tree
    where
    go Empty = False
    go (Node e _ left val right)
        | v < val   = go left
        | v > val   = go right
        | otherwise = e

insert' :: Ord a => a -> RBTree a -> RBTree a
insert' v tree = tree { nTotal = nTotal tree + 1, treeData = Node erv Black l rv r }
    where
    ins Empty = Node True Red Empty v Empty
    ins (Node eval c left val right)
        | v < val   = balance eval c (ins left) val right
        | v > val   = balance eval c left val (ins right)
        | otherwise = Node True c left val right
    Node erv _ l rv r = ins (treeData tree)

balance :: Ord a => Bool -> Color -> RBTreeData a -> a -> RBTreeData a -> RBTreeData a
balance ez Black (Node ey Red (Node ex Red a x b) y c) z d = Node ey Red (Node ex Black a x b) y (Node ez Black c z d)
balance ez Black (Node ex Red a x (Node ey Red b y c)) z d = Node ey Red (Node ex Black a x b) y (Node ez Black c z d)
balance ex Black a x (Node ez Red (Node ey Red b y c) z d) = Node ey Red (Node ex Black a x b) y (Node ez Black c z d)
balance ex Black a x (Node ey Red b y (Node ez Red c z d)) = Node ey Red (Node ex Black a x b) y (Node ez Black c z d)
balance ev c left v right = Node ev c left v right

delete' :: (Show a, Ord a) => a -> RBTree a -> RBTree a
delete' v tree = if nDeleted newTree <= nTotal newTree `div` 2
                    then newTree
                    else recreate newTree
    where
    newTree = tree { nDeleted = nDeleted tree + 1, treeData = newData }
    newData = go (treeData tree)
    go Empty = Empty
    go (Node e c left val right)
        | v < val = Node e c (go left) val right
        | v > val = Node e c left val (go right)
        | otherwise = Node False c left val right

recreate :: Show a => RBTree a -> RBTree a
recreate (RBTree _ _ root) = RBTree (genericLength xs) 0 (fromOrdList xs)
    where
    xs = go root
    go Empty = []
    go (Node e _ left v right) = if e then go left ++ v : go right else go left ++ go right

fromOrdList :: Show a => [a] -> RBTreeData a
fromOrdList xs = create 0 extras xs
    where
    nnodes = length xs
    height = finiteBitSize nnodes - countLeadingZeros nnodes
    blackHeight = if (1 `shiftL` height) - 1 == nnodes then height else height - 1
    extras = if blackHeight == height then 0 else nnodes - (1 `shiftL` blackHeight - 1)
    create _ _ [] = Empty
    create depth _ [val] | depth == blackHeight = Node True Red Empty val Empty
    create depth nreds vals = let subheight = blackHeight - depth - 1
                                  subblacks = (1 `shiftL` subheight) - 1
                                  lowblacks = 1 `shiftL` (subheight - 1)
                                  leftreds = if subheight == 0 then min nreds 1 else min nreds (lowblacks * 2)
                                  rightreds = nreds - leftreds
                                  (lefts, val : rights) = splitAt (subblacks + leftreds) vals
                                  left = create (depth + 1) leftreds lefts
                                  right = create (depth + 1) rightreds rights
                               in Node True Black left val right
