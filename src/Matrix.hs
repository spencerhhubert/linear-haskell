module Matrix where

import Useful
import Vector

type Matrix a = [[a]]

{-@ row :: m:(Matrix a) -> {v:Nat | v < (rows m)} -> Vector a @-}
row :: Matrix a -> Int -> Vector a
row x i = x !! i

column :: Matrix a -> Int -> Vector a
column x j = map (!! j) x

getValue :: Matrix a -> Int -> Int -> a
getValue x i j = (x !! i) !! j

setValue :: Matrix a -> Int -> Int -> a -> Matrix a
setValue x i j a = replace i (replace j a (row x i)) x 

transpose :: Matrix a -> Matrix a
transpose x = map (column x) [0..((snd $ dimensions x) - 1)]

dimensions :: Matrix a -> (Int, Int)
dimensions x = (length x, length (x !! 0))

dimension :: Matrix a -> Int
dimension x = (fst $ dimensions x) * (snd $ dimensions x)

rows :: Matrix a -> Int
rows x = fst $ dimensions x

columns :: Matrix a -> Int
columns x = snd $ dimensions x

multiply :: Num a => Matrix a -> Matrix a -> Matrix a
multiply x y
    | dimensions x == (1,1) = mapMatrix (\v -> v * (grabVal x)) y
    | dimensions y == (1,1) = mapMatrix (\v -> v * (grabVal y)) x
    | otherwise = group p products where
        m = rows x
        p = columns y
        products = [dot v w | v <- x, w <- (transpose y)]
        grabVal scalarMat = ((scalarMat !! 0) !! 0)

showMatrix :: Show a => Matrix a -> IO ()
showMatrix m = mapM_ putStrLn (map show m)

generateUniformMatrix :: Int -> Int -> Float -> Matrix Float
generateUniformMatrix m n x = group n values where
    values = [x * (fromIntegral $ v + w) | v <- [1..m], w <- [1..n]]

generateIntMatrix :: Int -> Int -> Matrix Int
generateIntMatrix m n = group n values where
    values = [v + w | v <- [1..m], w <- [1..n]]

generateFloatMatrix :: Int -> Int -> Matrix Float
generateFloatMatrix m n = group n values where
    values = [fromIntegral (v + w) | v <- [1..m], w <- [1..n]]

mapMatrix :: (a -> b) -> Matrix a -> Matrix b
mapMatrix f m = map (map f) m

addMatrix :: Num a => Matrix a -> Matrix a -> Matrix a
addMatrix x y = zipWith add x y

zipWithMatrix :: (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
zipWithMatrix f x y = zipWith (zipWith f) x y
