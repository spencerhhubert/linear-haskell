module Main where

import Tensor

ten00 = Value 999
ten01 = Value 7
ten02 = Value 3
ten03 = Tensor [ten01, ten02]
ten04 = Tensor [ten01, ten01, ten02, ten01]
ten05 = Tensor [ten03, ten04]
ten06 = Tensor [ten01]
ten07 = Tensor [ten01]

ten11 = Tensor $ map Value [1..8]
ten12 = Tensor $ map Value [9..16]
ten13 = Tensor $ map Value $ [43, 32]++[1..6]
ten14 = Tensor [ten11, ten12, ten13] --3x8 matrix
ten15 = Tensor $ map Value [16..23] --_x8
ten16 = ranTen [5,2,8,1,11]
ten17 = ranTen [1,2,8,9,11]
ten18 = snd $ bc (ten16, ten17)
ten19 = ranTen [3,8]
--               x8      3x8
ten20 = fst $ bc (ten11, ten19)

--from ten14 and ten15
desired_broadcast01 = (ten14, Tensor [ten15, ten15, ten15])

actual_broadcast01 = broadcast (ten14, ten15)

t1 = ten16
t2 = ten17
d1 = print $ show $ depth t1
d2 = print $ show $ depth t2
s1 = print $ show $ shape t1
s2 = print $ show $ shape t2
t1' = fst $ makeSameDepth (t1, t2)
t2' = snd $ makeSameDepth (t1, t2)
seet2' = print $ show t2'
d1' = print $ show $ depth t1'
d2' = print $ show $ depth t2'
s1' = print $ show $ shape t1'
s2' = print $ show $ shape t2'
t1'' = fst $ bc (t1, t2)
t2'' = snd $ bc (t1, t2)
b1 = print $ show t1''
b2 = print $ show t2''
sb1 = print $ show $ shape t1''
sb2 = print $ show $ shape t2''

main :: IO ()
main = do
    -- d1
    -- d2
    -- s1
    -- s2
    d1'
    d2'
    s1'
    s2'
    print $ show $ how $ makeSameDepth (t1, t2)
    sb1
    sb2
