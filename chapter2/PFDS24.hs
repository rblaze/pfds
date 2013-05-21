module PFDS24 where

import Data.Maybe

import PFDS2Set

insert24 :: Ord a => a -> Tree a -> Tree a
insert24 val tree = fromMaybe tree (go Nothing tree)
    where
    go m node@(Tree left key right)
        | val < key = do
                        t <- go m left
                        return (Tree t key right)
        | otherwise = do
                        t <- go (Just node) right
                        return (Tree left key t)

    go (Just (Tree _ key _)) Empty | key == val = Nothing
    go _ Empty = Just (Tree Empty val Empty)
