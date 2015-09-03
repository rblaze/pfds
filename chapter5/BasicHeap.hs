{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module BasicHeap where

class Ord a => Heap h a | h -> a where
    empty :: h
    insert :: a -> h -> h
    findMin :: h -> Maybe a
    deleteMin :: h -> h
    toList :: h -> [a]
    fromList :: [a] -> h
    fromList = foldr insert empty
