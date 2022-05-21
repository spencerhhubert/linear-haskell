module Linear where

type Vector a = [a]
type Matrix a = [[a]]

dot :: Num a => Vector a -> Vector a -> a
dot x y = sum (zipWith (*) x y)

add :: Num a => Vector a -> Vector a -> Vector a
add x y = zipWith (+) x y

scale :: Num a => Vector a -> a -> Vector a
scale x c = map (c *) x

row :: Matrix a -> Int -> Vector a
row x i = x !! i

column :: Matrix a -> Int -> Vector a
column x j = map (!! j) x

value :: Matrix a -> Int -> Int -> a
value x i j = (x !! i) !! j

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
multiply x y = group p products where
    m = rows x
    p = columns y
    products = [dot v w | v <- x, w <- (transpose y)]

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "Negative or zero n"

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
