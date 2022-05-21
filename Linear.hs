module Linear where

type Vector a = [a]
type Matrix a = [[a]]

dot :: Num a => Vector a -> Vector a -> a
dot x y = sum (zipWith (*) x y)

add :: Num a => Vector a -> Vector a -> Vector a
add x y = zipWith (+) x y

subtract :: Num a => Vector a -> Vector a -> Vector a
subtract x y = zipWith (-) x y

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
    group _ [] = []
    group n l
      | n > 0 = (take n l) : (group n (drop n l))
      | otherwise = error "Negative or zero n"