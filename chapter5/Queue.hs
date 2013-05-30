module Queue where

import Data.Foldable

data Queue a = Queue [a] [a]

instance Functor Queue where
    fmap f (Queue qin qout) = Queue (fmap f qin) (fmap f qout)

instance Foldable Queue where
    foldr f z (Queue qin qout) = foldl' (flip f) (Prelude.foldr f z qin) qout

empty :: Queue a
empty = Queue [] []

null :: Queue a -> Bool
null (Queue [] []) = True
null _ = False

push :: Queue a -> a -> Queue a 
push (Queue qin qout) v = Queue (v:qin) qout

pop :: Queue a -> Queue a
pop (Queue [] []) = error "Queue underrun (pop)" 
pop (Queue qin (_:xs)) = Queue qin xs
pop (Queue qin []) = pop (Queue [] (reverse qin))

head :: Queue a -> a
head (Queue [] []) = error "Queue underrun (head)"
head (Queue _ (x:_)) = x
head (Queue qin []) = Prelude.last qin

last :: Queue a -> a
last (Queue [] []) = error "Queue underrun (last)"
last (Queue (x:_) _) = x
last (Queue [] qout) = Prelude.last qout

length :: Queue a -> Int
length (Queue qin qout) = Prelude.length qin + Prelude.length qout

toList :: Queue a -> [a]
toList (Queue qin qout) = qout ++ reverse qin
