module Main where

import Tensor

ten01 = Value 7
ten02 = Value 3
ten03 = Tensor [1,2] [ten01, ten02]
ten04 = Tensor [4,1] [ten01, ten01, ten02, ten01]
ten05 = Tensor [4,1,3] [ten03, ten04]
ten06 = Tensor [1,5,3] [ten01]
ten07 = Tensor [7,5,4] [ten01]

main :: IO ()
main = do
    print $ show ten05
    print $ show $ dim ten05
    print $ show $ checkCompatibility ten05 ten06
    print $ show $ checkCompatibility ten06 ten07

