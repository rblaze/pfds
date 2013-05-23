module PFDS26 where

import Prelude hiding (lookup)

data Map k v = Empty
             | Map !(Map k v) !k v !(Map k v)

lookup :: Ord k => k -> Map k v -> Maybe v
lookup _ Empty = Nothing
lookup key (Map left k v right)
    | k < key       = lookup key left
    | k > key       = lookup key right
    | otherwise     = Just v

bind :: Ord k => k -> v -> Map k v -> Map k v
bind key val Empty = Map Empty key val Empty
bind key val node@(Map left k v right)
    | k < key       = Map (bind key val left) k v right
    | k > key       = Map left k v (bind key val right)
    | otherwise     = Map left key val right

toList :: Map k v -> [(k,v)]
toList Empty = []
toList (Map left key val right) = toList left ++ [(key, val)] ++ toList right
