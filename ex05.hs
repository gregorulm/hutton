{-
   "Graham Hutton: Programming in Haskell"
   Chapter 5: Solutions to exercises

   Gregor Ulm
-}

import Data.Char

-- 1. Sum of squares
sumSquares :: Integer
sumSquares = sum [ x * x | x <- [1..100]]

sumSquares' :: Integer -> Integer
sumSquares' n | n < 1         = error "bad arg"
              | otherwise     = sum [ x * x | x <- [1..n]]


-- 2. Replicate
replicate' :: Int -> a -> [a]
replicate' n x = [ x | _ <- [1..n]]


-- 3. Pythagorean triples
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n],
                          x * x + y * y == z * z]

-- 4. Perfect numbers
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [ x | x <- [1..n], isPerfect x ]
        where isPerfect n = sum (factors n) - n == n   

-- 5. Show equivalence
original = [(x, y) | x <- [1,2,3], y <- [4,5,6]]

rewrite  = concat [[ (x,y) | y <- [4,5,6]] | x <- [1,2,3] ]

result   = [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]

prop_equiv = (original == result) && (result == rewrite)


-- 6. Redefine "positions" using "find"
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs[0..length xs - 1], x == x']
-- positions 2 [1,2,3,3,2]
-- [1,4]

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']
-- find' 'b' [('a',1),('b',2),('c',3),('b',4)]
-- [2,4]

positions' x xs = find x xs'
        where xs' = zip [1..] xs
         

-- 7. Scalar product
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [ x * y | (x, y) <- zip xs ys ]


-- 8. Caesar cipher: handle upper-case letters as well

let2intLower :: Char -> Int
let2intLower c = ord c - ord 'a'

int2letLower :: Int -> Char
int2letLower n = chr $ ord 'a' + n

let2intUpper :: Char -> Int
let2intUpper c = ord c - ord 'A'

int2letUpper :: Int -> Char
int2letUpper n = chr $ ord 'A' + n

shift :: Int -> Char -> Char
shift n c | isLower c = int2letLower ((let2intLower c + n) `mod` 26)
          | isUpper c = int2letUpper ((let2intUpper c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
