{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances #-}
module PFDS37 where

import BasicHeap

data CachedHeap h a where
    CachedHeap :: (Heap h a) => h -> Maybe a -> CachedHeap h a

instance (Heap h a) => Heap (CachedHeap h a) a where
    empty = CachedHeap empty Nothing
    insert val (CachedHeap heap el) = CachedHeap (insert val heap) (Just $ maybe val (min val) el)
    findMin (CachedHeap _ el) = el
    deleteMin (CachedHeap heap _) = let newheap = deleteMin heap
                                    in CachedHeap newheap (findMin newheap)
    toList (CachedHeap heap _) = toList heap
