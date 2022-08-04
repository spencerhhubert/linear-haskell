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

--from ten14 and ten15
desired_broadcast01 = (ten14, Tensor [ten15, ten15, ten15])

actual_broadcast01 = broadcast (ten14, ten15)


main :: IO ()
main = do
--     print $ showTensor $ fst desired_broadcast01
--     putStrLn ""
--     print $ showTensor $ snd desired_broadcast01
--     putStrLn ""
--     print $ showTensor $ fst actual_broadcast01 
--     putStrLn ""
--     print $ showTensor $ snd actual_broadcast01
--     putStrLn ""
--     print $ showTensor $ fst (broadcast (ten11, ten01))
--     print $ showTensor $ snd (broadcast (ten11, ten01))
--     -- print $ show $ dim ten11
--     -- print $ show $ isBottom ten11
--     -- print $ show $ isBottom ten14
--     print $ show $ depth ten14
-- --    print $ show $ stepUp (ten14, ten01)
--     print $ show $ get ten14 [1]
--     print $ show $ dim ten14
--     print $ show $ dupe ten14 3
--     print $ show $ dupe ten01 4
    print $ showTensor ten14
    print $ showTensor (set ten14 ten00 [2,2])
