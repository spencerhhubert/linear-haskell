module Tensor where

import Matrix

--a tensor is a list of tensors. a tensor can also be a value
data Tensor a = Tensor {dim :: [Int], values :: [Tensor a]} | Value {value :: a}
    deriving Show

--compatibility for broadcast multiply
checkCompatibility :: Tensor a -> Tensor a -> Bool
checkCompatibility x y = all (==True) (zipWith (check) (dim x) (dim y)) where
    check a b = a == b || a == 1 || b == 1
