module BasicHeap where

class Heap h where
    empty :: h a
    insert :: (Ord a) => a -> h a -> h a
    findMin :: (Ord a) => h a -> Maybe a
    deleteMin :: (Ord a) => h a -> h a
    toList :: h a -> [a]
    fromList :: (Ord a) => [a] -> h a
    fromList = foldr insert empty
