module PFDS25 where

import PFDS2Set

mktree25a :: a -> Int -> Tree a
mktree25a el depth = go depth Empty
    where
    go 0 subtree = subtree
    go n subtree = go (n - 1) (Tree subtree el subtree)

mktree25b :: a -> Int -> Tree a
mktree25b el cnt = fst $ create2 cnt
    where
    create2 0 = (Empty, Tree Empty el Empty)
    create2 n | odd n = let (sub0, sub1) = create2 (n `div` 2)
                         in (Tree sub0 el sub0, Tree sub1 el sub0)
    create2 n = let (sub0, sub1) = create2 ((n `div` 2) - 1)
                 in (Tree sub0 el sub1, Tree sub1 el sub1)
