module Queue where

import Prelude hiding(tail)
import Data.Foldable

data Queue a = Queue [a] [a]

instance Functor Queue where
    fmap f (Queue qin qout) = Queue (fmap f qin) (fmap f qout)

instance Foldable Queue where
    foldr f z (Queue qin qout) = let tmp = Prelude.foldr f z qin
                                  in foldl' (flip f) tmp qout

empty :: Queue a
empty = Queue [] []

null :: Queue a -> Bool
null (Queue [] []) = True
null _ = False

snoc :: Queue a -> a -> Queue a 
snoc (Queue [] []) v = Queue [] [v]
snoc (Queue _ []) _ = error "invariant failed"
snoc (Queue qin qout) v = Queue (v:qin) qout

tail :: Queue a -> Queue a
tail (Queue [] []) = error "Queue underrun (pop)" 
tail (Queue qin (_:[])) = Queue [] (reverse qin)
tail (Queue qin (_:xs)) = Queue qin xs
tail (Queue _ _) = error "tail invariant failed"

head :: Queue a -> a
head (Queue [] []) = error "Queue underrun (head)"
head (Queue _ (x:_)) = x
head (Queue _ _) = error "head invariant failed"

last :: Queue a -> a
last (Queue [] []) = error "Queue underrun (last)"
last (Queue (x:_) _) = x
last (Queue [] qout) = Prelude.last qout

length :: Queue a -> Int
length (Queue qin qout) = Prelude.length qin + Prelude.length qout

toList :: Queue a -> [a]
toList (Queue qin qout) = qout ++ reverse qin
