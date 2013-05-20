module PFDS23 where

import Data.Maybe

import PFDS2Set

insert23 :: Ord a => a -> Tree a -> Tree a
insert23 val tree = fromMaybe tree (go tree)
    where
    go Empty = Just (Tree Empty val Empty)
    go (Tree left key right) = case compare val key of
                                    EQ -> Nothing
                                    LT -> case go left of
                                            Nothing -> Nothing
                                            Just t -> Just (Tree t key right)
                                    GT -> case go right of
                                            Nothing -> Nothing
                                            Just t -> Just (Tree left key t)
