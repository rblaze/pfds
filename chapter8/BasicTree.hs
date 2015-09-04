{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module BasicTree where

class Ord a => Tree t a | t -> a where
    empty :: t
    insert :: a -> t -> t
    member :: a -> t -> Bool
    delete :: a -> t -> t
