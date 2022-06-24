module Linear where
import System.Random

--will need to make these proper data types, but the synatax is so clean this way...
type Vector a = [a]
type Matrix a = [[a]]

-- class Tensor a where

dot :: Num a => Vector a -> Vector a -> a
dot x y = sum (zipWith (*) x y)

add :: Num a => Vector a -> Vector a -> Vector a
add x y = zipWith (+) x y

subtract :: Num a => Vector a -> Vector a -> Vector a
subtract x y = zipWith (-) x y

scale :: Num a => Vector a -> a -> Vector a
scale x c = map (c *) x

{-@ row :: m:(Matrix a) -> {v:Nat | v < (rows m)} -> Vector a @-}
row :: Matrix a -> Int -> Vector a
row x i = x !! i

column :: Matrix a -> Int -> Vector a
column x j = map (!! j) x

getValue :: Matrix a -> Int -> Int -> a
getValue x i j = (x !! i) !! j

setValue :: Matrix a -> Int -> Int -> a -> Matrix a
setValue x i j a = replace i (replace j a (row x i)) x 

replace :: Int -> a -> [a] -> [a]
replace pos newVal list = take pos list ++ newVal : drop (pos+1) list

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
group n l = (take n l) : (group n (drop n l)) 

show_matrix :: Show a => Matrix a -> IO ()
show_matrix m = mapM_ putStrLn (map show m)

generate_matrix :: Int -> Int -> Float -> Matrix Float
generate_matrix x y z = group y values where
    values = [z * (fromIntegral $ v + w) | v <- [1..x], w <- [1..y]]
