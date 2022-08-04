module Useful where

replace :: Int -> a -> [a] -> [a]
replace pos newVal list = take pos list ++ newVal : drop (pos+1) list

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l = (take n l) : (group n (drop n l)) 

takeTail :: Int -> [a] -> [a]
takeTail i l = reverse (take i $ reverse l)

--imagine you got the part you didn't take from a list if you did take on a list
reverseTake :: Int -> [a] -> [a]
reverseTake i l = takeTail ((length l) - i) l

setAtList :: Int -> a -> [a] -> [a]
setAtList i x l = (take i l) ++ [x] ++ (tail $ reverseTake i l)
