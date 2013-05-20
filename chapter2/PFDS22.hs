module PFDS22 where

import PFDS2Set

member22 :: Ord a => a -> Tree a -> Bool
member22 val = go Nothing
    where
    go (Just (Tree _ key _)) Empty = val == key
    go _ Empty = False
    go m node@(Tree left key right)
        | val < key     = go m left
        | otherwise     = go (Just node) right
