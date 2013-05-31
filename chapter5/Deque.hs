module Deque where

import Prelude hiding(tail)
import Data.Foldable

data Deque a = Deque [a] [a]
    deriving Show

instance Functor Deque where
    fmap f (Deque qin qout) = Deque (fmap f qin) (fmap f qout)

instance Foldable Deque where
    foldr f z (Deque qin qout) = let tmp = Prelude.foldr f z qin
                                  in foldl' (flip f) tmp qout

fixdeq :: Deque a -> Deque a
fixdeq d@(Deque [] []) = d
fixdeq d@(Deque (_:_) (_:_)) = d
fixdeq d@(Deque (_:[]) []) = d
fixdeq d@(Deque [] (_:[])) = d
fixdeq (Deque qout []) = let (qout', qin) = splitAt (Prelude.length qout `div` 2) qout
                          in Deque qout' (reverse qin)
fixdeq (Deque [] qin) = let (qin', qout) = splitAt (Prelude.length qin `div` 2) qin
                         in Deque (reverse qout) qin'

empty :: Deque a
empty = Deque [] []

null :: Deque a -> Bool
null (Deque [] []) = True
null _ = False

snoc :: Deque a -> a -> Deque a 
snoc (Deque qout qin) v = fixdeq $ Deque qout (v:qin)

cons :: a -> Deque a -> Deque a
cons v (Deque qout qin) = fixdeq $ Deque (v:qout) qin

tail :: Deque a -> Deque a
tail (Deque [] []) = error "Deque underrun (tail)" 
tail (Deque (_:xs) qin) = fixdeq $ Deque xs qin
tail (Deque [] (_:[])) = Deque [] []
tail (Deque _ _) = error "tail invariant failed"

init :: Deque a -> Deque a
init (Deque [] []) = error "Deque underrun (init)" 
init (Deque qout (_:xs)) = fixdeq $ Deque qout xs
init (Deque (_:[]) []) = Deque [] []
init (Deque _ _) = error "init invariant failed"

head :: Deque a -> a
head (Deque [] []) = error "Deque underrun (head)"
head (Deque (x:_) _) = x
head (Deque [] (x:[])) = x
head (Deque _ _) = error "head invariant failed"

last :: Deque a -> a
last (Deque [] []) = error "Deque underrun (last)"
last (Deque _ (x:_)) = x
last (Deque (x:[]) []) = x
last (Deque _ _) = error "last invariant failed"

length :: Deque a -> Int
length (Deque qout qin) = Prelude.length qin + Prelude.length qout

toList :: Deque a -> [a]
toList (Deque qout qin) = qout ++ reverse qin
