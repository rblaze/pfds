module PFDS21 where

suffixes :: [a] -> [[a]]
suffixes xxs@(_:xs) = xxs : suffixes xs
suffixes [] = [[]]
