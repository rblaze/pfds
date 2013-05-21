module PFDS23 where

import Data.Maybe

import PFDS2Set

insert23 :: Ord a => a -> Tree a -> Tree a
insert23 val tree = fromMaybe tree (go tree)
    where
    go Empty = Just (Tree Empty val Empty)
    go (Tree left key right)
        | val < key = do
                        t <- go left
                        return (Tree t key right)
        | val > key = do
                        t <- go right
                        return (Tree left key t)
        | otherwise = Nothing
