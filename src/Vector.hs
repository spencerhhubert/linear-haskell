module Vector where

import Useful

type Vector a = [a]

dot :: Num a => Vector a -> Vector a -> a
dot x y = sum (zipWith (*) x y)

add :: Num a => Vector a -> Vector a -> Vector a
add x y = zipWith (+) x y

sub :: Num a => Vector a -> Vector a -> Vector a
sub x y = zipWith (-) x y

scale :: Num a => Vector a -> a -> Vector a
scale x c = map (c *) x


