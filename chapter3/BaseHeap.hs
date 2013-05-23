module BaseHeap where

data Heap a = Empty
            | Heap !Int !a !(Heap a) !(Heap a)


merge :: Ord a => Heap a -> Heap a -> Heap a
merge Empty heap = heap
merge heap Empty = heap
merge left@(Heap _ lval ll lr) right@(Heap _ rval rl rr)
    | lval < rval   = makeT lval ll $ merge lr right
    | otherwise     = makeT rval rl $ merge left rr
  where
    rank :: Heap a -> Int
    rank Empty = 0
    rank (Heap r _ _ _) = r

    makeT val ltree rtree
        | rank ltree >= rank rtree  = Heap (rank rtree + 1) val ltree rtree
        | otherwise                 = Heap (rank ltree + 1) val rtree ltree

insert :: Ord a => a -> Heap a -> Heap a
insert val = merge (Heap 1 val Empty Empty)

findMin :: Heap a -> Maybe a
findMin Empty = Nothing
findMin (Heap _ val _ _) = Just val

deleteMin :: Ord a => Heap a -> Heap a
deleteMin Empty = error "heap underflow"
deleteMin (Heap _ _ left right) = merge left right

toList :: Heap a -> [a]
toList Empty = []
toList (Heap _ val left right) = val : toList left ++ toList right
