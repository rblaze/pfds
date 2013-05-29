module PFDS39 where

import RBTree

import Data.Bits

fillInt :: Int -> Int -> Int
fillInt ret 0 = ret
fillInt ret v = let s = v `shiftR` 1 in fillInt (ret .|. s) s

fromOrdList :: Ord a => [a] -> RedBlackTree a
fromOrdList xs = RedBlackTree tree
    where
    nelems = length xs
    filled = fillInt nelems nelems
    depth = popCount filled
    extra = if nelems == filled then 0 else nelems - (filled `shiftR` 1)
    (tree, _, _) = create (if extra == 0 then depth else depth - 1) (0 :: Int) xs
    create 0 reds ys | reds == extra = (Empty, reds, ys)
    create _ _ [] = error "list underflow"
    create 0 reds (y:ys) = (Node Red Empty y Empty, reds + 1, ys)
    create n reds ys = let (left, r1s, v:rs) = create (n - 1) reds ys
                           (right, r2s, rest) = create (n - 1) r1s rs
                        in (Node Black left v right, r2s, rest)
