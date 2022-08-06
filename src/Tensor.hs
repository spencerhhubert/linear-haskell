module Tensor where

import Useful
import MyRandom

--a tensor is a list of tensors. a tensor can also bottom out at a value
data Tensor a = Tensor {values :: [Tensor a]} | Value {value :: a}
    deriving (Eq, Show)

showTensor :: Show a => Tensor a -> String
showTensor (Value val) = show val
showTensor (Tensor tens) = show $ map showTensor tens where
    showPrettyList l = map read l --list of ints

--the minimum number of indices required to get a value
shape :: Tensor a -> [Int]
shape (Value val) = []
shape (Tensor ten) = (length ten) : (shape $ head ten)

dim :: Tensor a -> Int
dim (Value val) = 0
dim (Tensor ten) = length $ shape $ Tensor ten

--get a tensor within a tensor at index d, which equates to "depth"
get :: Tensor a -> [Int] -> Tensor a
get (Value val) _ = Value val
get (Tensor tens) is
    | length is > depth (Tensor tens) = error "invalid tensor indexing"
    | length is == 1 = (!!) tens $ head is
    | otherwise = get ((!!) tens $ head is) $ tail is

--set the tensor at a certain coordinate
set :: Tensor a -> Tensor a -> [Int] -> Tensor a
set (Value val) x _ = x
set (Tensor tens) x [] = error "idk"
set (Tensor tens) x (i:[]) = Tensor $ setAtList i x tens
set (Tensor tens) x (i:is) = Tensor $ setAtList i (set (tens !! i) x is) tens

isBottom :: Tensor a -> Bool
isBottom ten = (dim ten) == 1

depths :: (Tensor a, Tensor b) -> [Int]
depths (a,b) = reverse [1..(max (depth a) (depth b))]

--this assumes the tensors are same depth
how :: (Tensor a, Tensor b) -> ([Int], [Int])
how (a,b) = unzip $ zipWith compare (shape a) (shape b) where
    compare :: Int -> Int -> (Int, Int)
    compare c d
        | c == d = (0,0)
        | c == 1 = (d,0)
        | d == 1 = (0,c)
        | otherwise = error "Not broadcastable"

--turns empty layers to ones
makeSameDepth :: (Tensor a, Tensor b) -> (Tensor a, Tensor b)
makeSameDepth (a,b)
        | depth a < depth b = (grow a times, b)
        | otherwise = (a, grow b times) --if they're the same length, times will be zero so it doesn't matter which one we apply it to
        where
            grow :: Tensor a -> Int -> Tensor a
            grow (Value val) 1 = Tensor [Value val]
            grow ten 0 = ten
            grow ten 1 = Tensor [ten]
            grow ten i = Tensor [grow ten $ i-1]
            times :: Int
            times = abs $ (depth a) - (depth b)

broadcast :: (Tensor a, Tensor b) -> (Tensor a, Tensor b)
broadcast (a,b) = (c,d) where
    expanded = makeSameDepth (a,b)
    a' = fst expanded
    b' = snd expanded
    c = step a' $ fst $ how $ expanded
    d = step b' $ snd $ how $ expanded --glitches becasue we pass can pass a Value to step and then a list of how it out to be broadcast
    step :: Tensor a -> [Int] -> Tensor a
    step (Value val) (i:[]) = dupe (Value val) i
    step (Value val) (i:is) = error "Not broadcastable"
    step ten [] = ten
    step ten (i:[]) = dupe ten i 
    step ten (i:is) = dupe (Tensor (map (\x -> step x is) $ values ten)) i --this should start from the "bottom", ie the right most dimension

dupe :: Tensor a -> Int -> Tensor a
dupe (Value val) times = Tensor (take times $ repeat (Value val))
dupe ten 0 = ten --jank. should "how" return a 1?
dupe ten times = Tensor (take times $ repeatList $ values ten)

dupe2 :: Tensor a -> Int -> Tensor a
dupe2 ten times = Tensor $ take times $ repeat ten

--reverse is because we check for broadcastability from the innermost dimension first
canBroadcast :: Tensor a -> Tensor a -> Bool
canBroadcast x y = all (==True) (zipWith check (reverse $ shape x) (reverse $ shape y)) where
    check a b = a == b || a == 1 || b == 1

depth :: Tensor a -> Int
depth (Value _) = 0
depth ten = 1 + (depth $ head $ values ten)

--generate random float tensor from shape
ranTen :: [Int] -> Tensor Float
ranTen (x:[]) = ran1dTen x where
    ran1dTen :: Int -> Tensor Float
    ran1dTen l = Tensor $ map (\x -> Value $ rf x) [1..l]
    rf x = randomFloat x (0,10)
ranTen (x:xs) = dupe2 (ranTen xs) x where

zipWithTen :: (a -> b -> c) -> Tensor a -> Tensor b -> Tensor c
zipWithTen f (Value a) (Value b) = Value $ f a b
zipWithTen f a b
    | (dim a) /= (dim b) = zipWithTen f (fst $ broadcast (a,b)) (snd $ broadcast (a,b))
    | otherwise = Tensor $ zipWith (zipWithTen f) (values a) (values b)

hadamard :: Num a => Tensor a -> Tensor a -> Tensor a
hadamard x y = zipWithTen (*) x y

add :: Num a => Tensor a -> Tensor a -> Tensor a
add x y = zipWithTen (+) x y