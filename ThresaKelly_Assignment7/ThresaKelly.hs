-- Author: Thresa Kelly.
-- Date: 11/13/23.
-- File: EECS 468 Assignment 7.
-- Description: define several functions
-- Inputs: replicate, perfects, find, positions, and scalarproduct functions
-- Outputs: return values of the input functions
-- Sources: Referenced the lecture slides.

-- produces a list of identical elements using list comprehension.
replicate' :: Int -> a -> [a]
replicate' n val = if n <= 0 then [] else [val | _ <- [1 .. n]] -- empty list if less than zero. Otherwise, return list of n items

-- returns the list of all perfect numbers up to a given limit using a list comprehension
perfects :: Int -> [Int]
perfects n = [x | x <- [1 .. n], sum (init (factors x)) == x] -- gets factors, then adds them

-- Returns factors of n. used by perfects
factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0] -- getting list 1 to nand checking for factors for each number.

-- returns the list of all values that are associated with a given key in a table.
find :: Eq a => a -> [(a, b)] -> [b]
find key points = [y | (x, y) <- points, key == x] -- key==x is True for matched key, gets indecis of match.

-- returns incices where key matches a list item
positions :: Eq a => a -> [a] -> [Int]
positions key xs = find key (zip xs [0 .. (length xs - 1)]) -- make list of points where keys are xs argument. Then use find to get indices.

-- returns the scalar product of two lists of integers xs and ys of length n is given by the sum of the products of the corresponding integers
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys] -- zip matches xs and ys together. Multiply each x by y. Then calculate sum.

-- ===== EXAMPLES =====
-- replicate' 3 True
-- [True, True, True]
-- perfects 500
-- [6,28,496]
-- find 'b' [('a',1),('b',2),('c',3),('b',4)]
-- [2,4]
-- positions 0 [1,0,0,1,0,1,1,0]
-- [1,2,4,7]
-- scalarproduct [1,2,3] [4,5,6]
-- 32

-- ===== TEST =====
-- replicate' 5 "test code"
-- ["test code","test code","test code","test code","test code"]
-- perfects 9000
-- [6,28,486,8128]
-- positions 1 [1,0,0,1,0,1,1,0]
-- [0,3,5,6]
-- scalarproduct [-1,2,3] [-4,-5,6]
-- 12