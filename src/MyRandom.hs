module MyRandom where

import System.Random
import Vector
import Matrix

--random float in range
randomFloat :: Int -> (Float, Float) -> Float
randomFloat gen (min, max) = ((*) initial $ max - min) + min where
    initial = fst (random (mkStdGen gen)) :: Float

randomFloatVector :: Int -> Vector Float
randomFloatVector len = map (\x -> randomFloat x (0,10)) [1..len]

randomFloatMatrix :: Int -> Int -> Matrix Float
randomFloatMatrix m n = mapMatrix (\x -> randomFloat x (0,10)) (generateIntMatrix m n)
