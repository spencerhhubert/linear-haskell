module Useful where

replace :: Int -> a -> [a] -> [a]
replace pos newVal list = take pos list ++ newVal : drop (pos+1) list

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l = (take n l) : (group n (drop n l)) 
