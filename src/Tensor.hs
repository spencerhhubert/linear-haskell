module Tensor where

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
set (Value val) b _ = b
set (Tensor tens) b is = Tensor 

--reverse is because we check for broadcastability from the innermost dimension first
canBroadcast :: Tensor a -> Tensor a -> Bool
canBroadcast x y = all (==True) (zipWith check (reverse $ shape x) (reverse $ shape y)) where
    check a b = a == b || a == 1 || b == 1

isBottom :: Tensor a -> Bool
isBottom ten = (dim ten) == 1

--2x2x3
--[
-- [[3,4,5],[9,3,2]],
-- [[1,2,3],[3,2,3]]
--]

depths :: (Tensor a, Tensor b) -> [Int]
depths (a,b) = reverse [1..(max (depth a) (depth b))]

--stepUp :: (Tensor a, Tensor b) -> [Int]
--stepUp (a,b) = [once (a,b) i | i <- depths (a,b)] where
--    once (c,d) i = broadcastOneStep ()


clone (a,b)
    | depth a < depth b = (grow a b,b)
    | depth a > depth b = (a,grow b a)
    | otherwise = (a,b)
    where
        grow x y = x

dupe :: Tensor a -> Int -> Tensor a
dupe ten times = Tensor (take times $ repeat ten)

broadcast :: (Tensor a, Tensor b) -> (Tensor a, Tensor b)
broadcast (a,b) = step (a,b) $ depths (a,b) where
        step :: (Tensor a, Tensor b) -> [Int] -> (Tensor a, Tensor b)
        step (c,d) [] = (c,d)
        step (c,d) i:is = step (onecast (c,d) i) is
        onecast :: (Tensor a, Tensor b) -> Int -> (Tensor a, Tensor b)
        onecast (c,d) i
            | shape c == shape d = (c,d)
            | head shape c
        


--1x2x1
--[
--[[4],[1]]
--]

--4x2x4
--[
--[[,,,],[,,,]]
--[[,,,],[,,,]]
--[[,,,],[,,,]]
--[[,,,],[,,,]]
--]

--should begin with broadcasting innermost value
--this is hard because it could be arbitrarily deep

--how do we "start" from the innermost value, where it is actually a value

--how do we jump to each of their lowest depths

--they don't even need to have the same number of dimensions, so if one has no dimension there, duplicate the values of the other

testten01 = Value 7
testten02 = Value 3
testten03 = Tensor [testten01, testten02]
testten04 = Tensor [testten01, testten01, testten02, testten01]
testten05 = Tensor [testten03, testten04]
testten06 = Tensor [testten01]
testten07 = Tensor [testten01]



broadcast = broadcastOneStep

broadcastOneStep :: (Tensor a, Tensor a) -> (Tensor a, Tensor a)
broadcastOneStep (x,y)
    | shape x == shape y = (x,y)
    | depth x == 0 = (stampOut x y, y)
    | depth y == 0 = (x, stampOut y x)
    | depth x == 1 = (stampOut x y, y)
    | depth y == 1 = (x, stampOut y x)
    | otherwise = error "Not broadcastable"
    where
        stampOut :: Tensor a -> Tensor a -> Tensor a
        stampOut a b = Tensor (take (length $ values b) $ repeat a)

depth :: Tensor a -> Int
depth (Value _) = 0
depth ten = 1 + (depth $ head $ values ten)
