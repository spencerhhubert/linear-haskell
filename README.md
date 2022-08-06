# linear-haskell
A Haskell library for tensors and linear algebra.

it's so pythonic!

Right now the library is split into two parts. The first has a `type Vector a = [a]` and `type Matrix a = [[a]]` and operations for these types. This is practically useless with the existence of the other half, which just has a datatype called `tensor`.

Tensors work like n-dimensional arrays. A tensor can either be a `Value a` or a list of type `Tensor a`. So a tensor can have as much "depth" as you'd like (a tensor of shape [3,2,8] has a depth of 8) and it always bottoms out with `Value`s of type `a`.

The two most important functions that operate on tensors are `broadcast` and `zipWithTen`. These functions do what you'd expect them to. With them we can construct `add` and `hadamard` (multiplication) like `add x y = zipWithTen (+) x y`.