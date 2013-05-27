module BinominalHeap where

import BasicHeap

data BiTree a = Node !Int a ![BiTree a]

type BiHeap a = [BiTree a]

newtype BinomHeap a = BinomHeap (BiHeap a)

instance Heap BinomHeap where
    empty = BinomHeap []
    insert val (BinomHeap heap) = BinomHeap $ insert' val heap
    findMin (BinomHeap heap) = findMin' heap
    deleteMin (BinomHeap heap) = BinomHeap $ deleteMin' heap
    toList (BinomHeap heap) = toList' heap

link :: (Ord a) => BiTree a -> BiTree a -> BiTree a
link (Node rank1 _ _) (Node rank2 _ _) | rank1 /= rank2 = error "rank mismatch in link"
link a@(Node rank vA treeA) b@(Node _ vB treeB)
    | vA <= vB  = Node (rank + 1) vA (b : treeA)
    | otherwise = Node (rank + 1) vB (a : treeB)

merge :: (Ord a) => BiHeap a -> BiHeap a -> BiHeap a
merge a [] = a
merge [] b = b
merge xss@(x@(Node rx _ _) : xs) yss@(y@(Node ry _ _) : ys)
    | rx < ry   = x : merge xs yss
    | rx > ry   = y : merge xss ys
    | otherwise = insertTree (link x y) (merge xs ys)

toList' :: BiHeap a -> [a]
toList' [] = []
toList' (Node _ v ys : xs) = v : toList' ys ++ toList' xs

insert' :: (Ord a) => a -> BiHeap a -> BiHeap a
insert' v = insertTree (Node 0 v [])

insertTree :: (Ord a) => BiTree a -> BiHeap a -> BiHeap a
insertTree tree [] = [tree]
insertTree tree@(Node rank1 _ _) heap@(t@(Node rank2 _ _) : ts)
    | rank1 < rank2 = tree : heap
    | otherwise     = insertTree (link tree t) ts

findMin' :: (Ord a) => BiHeap a -> Maybe a
findMin' [] = Nothing
findMin' heap = Just $ minimum $ map (\(Node _ v _) -> v) heap

deleteMin' :: (Ord a) => BiHeap a -> BiHeap a
deleteMin' [] = error "biheap underflow"
deleteMin' heap = merge (reverse children) subheap
    where
    (children, subheap) = let (Node _ _ ch, s) = findmin heap in (ch, s)
    findmin [] = undefined
    findmin (n : []) = (n, [])
    findmin (n@(Node _ v _) : xs) = let (n'@(Node _ v' _), xs') = findmin xs
                                      in if v < v' then (n, xs) else (n', n : xs')
