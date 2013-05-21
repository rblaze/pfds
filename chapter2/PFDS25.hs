module PFDS25 where

import PFDS2Set

mktree25a :: a -> Int -> Tree a
mktree25a el depth = go depth Empty
    where
    go 0 subtree = subtree
    go n subtree = go (n - 1) (Tree subtree el subtree)

