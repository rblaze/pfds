module RBTree where

data Color = Red | Black
    deriving Eq
data Tree a = Empty
            | Node !Color !(Tree a) a !(Tree a)

member :: (Ord a) => a -> Tree a -> Bool
member _ Empty = False
member v (Node _ left val right)
    | v < val   = member v left
    | v > val   = member v right
    | otherwise = True

insert :: (Ord a) => a -> Tree a -> Tree a
insert v tree = Node Black l rv r
    where
    ins Empty = Node Red Empty v Empty
    ins n@(Node c left val right)
        | v < val   = balance c (ins left) val right
        | v > val   = balance c left val (ins right)
        | otherwise = n
    Node _ l rv r = ins tree

balance :: (Ord a) => Color -> Tree a -> a -> Tree a -> Tree a
balance Black (Node Red (Node Red a x b) y c) z d = Node Red (Node Black a x b) y (Node Black c z d)
balance Black (Node Red a x (Node Red b y c)) z d = Node Red (Node Black a x b) y (Node Black c z d)
balance Black a x (Node Red (Node Red b y c) z d) = Node Red (Node Black a x b) y (Node Black c z d)
balance Black a x (Node Red b y (Node Red c z d)) = Node Red (Node Black a x b) y (Node Black c z d)
balance c left v right = Node c left v right
