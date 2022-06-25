# linear-haskell
A Haskell linear algebra library

it's so pythonic!

this will need to be updated with proper data types but right now with vectors synonymous with lists and matrices being vectors of vectors, the syntax is really nice.

we very simply define vector and matrix as:

	type Vector a = [a]
	type Matrix a = [[a]]

and then we get all the great build in functions for list processing in haskell. like, for example, applying a function to everything in the matrix is as simply as:

	mapMatrix f m = map (map f) m
