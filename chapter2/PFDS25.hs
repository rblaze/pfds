module PFDS25 where

import PFDS2Set

mktree25a :: a -> Int -> Tree a
mktree25a el depth = go depth Empty
    where
    go 0 subtree = subtree
    go n subtree = go (n - 1) (Tree subtree el subtree)

mktree25b :: a -> Int -> Tree a
mktree25b el cnt
    | cnt == 0  = Empty
    | odd cnt   = let half = cnt `div` 2
                      subtree = mktree25b el half
                   in Tree subtree el subtree
    | otherwise = let half = cnt `div` 2
                      left = mktree25b el half
                      right = mktree25b el (half - 1)
                   in Tree left el right
