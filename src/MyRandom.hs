module MyRandom where

import System.Random
import Vector
import Matrix

randomFloat :: Int -> Float
randomFloat gen = fst (random (mkStdGen gen)) :: Float

randomFloatVector :: Int -> Vector Float
randomFloatVector len = map randomFloat [1..len]

randomFloatMatrix :: Int -> Int -> Matrix Float
randomFloatMatrix m n = mapMatrix randomFloat (generateIntMatrix m n)
