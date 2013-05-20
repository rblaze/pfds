module PFDS2Set where

data Tree a = Empty
            | Tree !(Tree a) !a !(Tree a)

member :: Ord a => a -> Tree a -> Bool
member _ Empty = False
member val (Tree left key right)
    | val < key     = member val left
    | val > key     = member val right
    | otherwise     = True

insert :: Ord a => a -> Tree a -> Tree a
insert val Empty = Tree Empty val Empty
insert val node@(Tree left key right)
    | val < key     = Tree (insert val left) key right
    | val > key     = Tree left key (insert val right)
    | otherwise     = node
