module PFDS36 where

import BasicHeap

data BiTree a = Node a ![BiTree a]

type BiHeap a = [(Int, BiTree a)]

newtype BinomHeap a = BinomHeap (BiHeap a)

instance Heap BinomHeap where
    empty = BinomHeap []
    insert val (BinomHeap heap) = BinomHeap $ insert' val heap
    findMin (BinomHeap heap) = findMin' heap
    deleteMin (BinomHeap heap) = BinomHeap $ deleteMin' heap
    toList (BinomHeap heap) = toList' heap

link :: (Ord a) => BiTree a -> BiTree a -> BiTree a
link a@(Node vA treeA) b@(Node vB treeB)
    | vA <= vB  = Node vA (b : treeA)
    | otherwise = Node vB (a : treeB)

merge :: (Ord a) => BiHeap a -> BiHeap a -> BiHeap a
merge a [] = a
merge [] b = b
merge xss@(x@(rx, nx) : xs) yss@(y@(ry, ny) : ys)
    | rx < ry   = x : merge xs yss
    | rx > ry   = y : merge xss ys
    | otherwise = insertTree rx (link nx ny) (merge xs ys)

toList' :: BiHeap a -> [a]
toList' = concatMap (\(_, n) -> step n)
    where
    step (Node v ys) = v : concatMap step ys

insert' :: (Ord a) => a -> BiHeap a -> BiHeap a
insert' v = insertTree 0 (Node v [])

insertTree :: (Ord a) => Int -> BiTree a -> BiHeap a -> BiHeap a
insertTree rank tree [] = [(rank, tree)]
insertTree rank1 tree heap@((rank2, nt) : ts)
    | rank1 < rank2 = (rank1, tree) : heap
    | otherwise     = insertTree (rank1 + 1) (link tree nt) ts

findMin' :: (Ord a) => BiHeap a -> Maybe a
findMin' [] = Nothing
findMin' heap = Just $ minimum $ map (\(_, Node v _) -> v) heap

deleteMin' :: (Ord a) => BiHeap a -> BiHeap a
deleteMin' [] = error "biheap underflow"
deleteMin' heap = merge (reverse $ zip [0 ..] children) subheap
    where
    (children, subheap) = let (Node _ ch, s) = findmin heap in (ch, s)
    findmin [] = undefined
    findmin ((_, n) : []) = (n, [])
    findmin ((r, n@(Node v _)) : xs) = let (n'@(Node v' _), xs') = findmin xs
                                        in if v < v' then (n, xs) else (n', (r, n) : xs')
