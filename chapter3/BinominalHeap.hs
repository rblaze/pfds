module BinominalHeap where

import BasicHeap

data BiTree a = Node !Int a ![BiTree a]

type BiHeap a = [BiTree a]

newtype BinomHeap a = BinomHeap (BiHeap a)

instance Heap BinomHeap where
    empty = BinomHeap []
    insert val (BinomHeap heap) = BinomHeap $ biInsert val heap
    findMin (BinomHeap heap) = biFindMin heap
    deleteMin (BinomHeap heap) = BinomHeap $ biDeleteMin heap
    toList (BinomHeap heap) = biToList heap

biLink :: (Ord a) => BiTree a -> BiTree a -> BiTree a
biLink (Node rank1 _ _) (Node rank2 _ _) | rank1 /= rank2 = error "rank mismatch in link"
biLink a@(Node rank vA treeA) b@(Node _ vB treeB)
    | vA <= vB  = Node (rank + 1) vA (b : treeA)
    | otherwise = Node (rank + 1) vB (a : treeB)

biMerge :: (Ord a) => BiHeap a -> BiHeap a -> BiHeap a
biMerge a [] = a
biMerge [] b = b
biMerge xss@(x@(Node rx _ _) : xs) yss@(y@(Node ry _ _) : ys)
    | rx < ry   = x : biMerge xs yss
    | rx > ry   = y : biMerge xss ys
    | otherwise = biInsertTree (biLink x y) (biMerge xs ys)

biToList :: BiHeap a -> [a]
biToList [] = []
biToList ((Node _ v ys):xs) = v : biToList ys ++ biToList xs

biInsert :: (Ord a) => a -> BiHeap a -> BiHeap a
biInsert v = biInsertTree (Node 0 v [])

biInsertTree :: (Ord a) => BiTree a -> BiHeap a -> BiHeap a
biInsertTree tree [] = [tree]
biInsertTree tree@(Node rank1 _ _) heap@(t@(Node rank2 _ _) : ts)
    | rank1 < rank2 = tree : heap
    | otherwise     = biInsertTree (biLink tree t) ts

biFindMin :: (Ord a) => BiHeap a -> Maybe a
biFindMin [] = Nothing
biFindMin heap = Just $ minimum $ map (\(Node _ v _) -> v) heap

biDeleteMin :: (Ord a) => BiHeap a -> BiHeap a
biDeleteMin [] = error "biheap underflow"
biDeleteMin heap = biMerge (reverse children) subheap
    where
    (children, subheap) = let ((Node _ _ ch), s) = findmin heap in (ch, s)
    findmin [] = undefined
    findmin (n : []) = (n, [])
    findmin (n@(Node _ v _) : xs) = let (n'@(Node _ v' _), xs') = findmin xs
                                      in if v < v' then (n, xs) else (n', n : xs')
