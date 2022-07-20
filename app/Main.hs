module Main where

import Vector
import Matrix
import MyRandom

vec1 = [11..15]
vec2 = [12..16]
vec3 = [101..105]

vec10 = [1..2]
vec11 = [3..4]
vec12 = [9..10]
vec13 = [13..14]
vec14 = [9..10]

mat1 = [vec1, vec2, vec3]
mat2 = [vec10, vec11, vec12, vec13, vec14]
mat3 = multiply mat1 mat2
mat4 = [vec1, vec3]

mat41 = [vec1]
mat42 = multiply mat1 mat10

-- mat1 - 3x3:
-- [
--     [11,12,13],
--     [12,13,14],
--     [101,102,103]
-- ]

-- mat2 - 3x3:
-- [
--     [101,102,103],
--     [11,12,13],
--     [12,13,14]
-- ]

-- mat3 - 3x3:
-- [
--     [1399,1435,1471]
--     [1523,1562,1601]
--     [12559,12865,13171]
-- ]

mat5 = generateFloatMatrix 100 42
mat6 = generateFloatMatrix 42 8
mat7 = multiply mat4 mat5

mat8 = randomFloatMatrix 3 4
mat9 = randomFloatMatrix 4 72
mat10 = multiply mat8 mat9

mat20 = [[3.8]]
mat21 = [[1.2, 9.8], [345.23, 123.0], [67.2, 99.9]]

main :: IO ()
main = do
	showMatrix $ multiply mat20 mat21
	showMatrix mat7
