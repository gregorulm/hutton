{-
   "Graham Hutton: Programming in Haskell"
   Chapter 2: Solutions to exercises

   Gregor Ulm
-}

-- 1. Parenthesize expressions
{-
(2 ^ 3) * 4
(2 * 3) + (4 * 5)
2 + (3 * (4 ^ 5)) 
-}

-- 2. Work through examples using Hugs interpreter
-- N/A

-- 3. Fix a function definition
n :: Int
n = a `div` length xs
        where a  = 10
              xs = [1,2,3,4,5]
{-
   The required fixes are:
        - function name has to be lower case
        - div has to be enclosed in backticks
        - in the where clause, the variable names have 
                to be aligned to the left
-}

-- 4. Define "last"
last2 :: [a] -> a
last2 []     = error "empty list"
last2 [x]    = x
last2 (x:xs) = last2 xs

last3 :: (Eq a) => [a] -> a
last3 xs | xs == []       = error "empty list"
         | length xs >  1 = last3 (tail xs)
         | length xs == 1 = head xs

-- 5. Define "init"
init2 :: [a] -> [a]
init2 []       = error "empty list"
init2 [x]      = []
init2 (x:xs)   = x : init2 xs

init3 :: (Eq a) => [a] -> [a]
init3 xs | xs == []        = error "empty list"  
         | length xs == 1  = []
         | otherwise       = head xs : init3 (tail xs)
