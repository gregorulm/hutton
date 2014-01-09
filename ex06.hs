{-
   "Graham Hutton: Programming in Haskell"
   Chapter 6: Solutions to exercises

   Gregor Ulm
-}


-- 1. Define exponentiation operator
-- Note: "^^" is defined in the Prelude and raises a number to an
-- integral power
(^^^) :: Integer -> Integer -> Integer
a ^^^ 0 = 1
a ^^^ b = a * (a ^^^ (b - 1))

{-
  Sample evaluation:
        2 ^^^ 3
      = 2 * (2 ^^^ 2)
      = 2 * 2 * (2 ^^^ 1)
      = 2 * 2 * 2 * (2 ^^^ 0)
      = 2 * 2 * 2 * 1
      = 8
-}


-- 2. Evaluate "length", "drop", "init"
{-
        length [1,2,3]
      = 1 + length [2,3]
      = 1 + 1 + length [3]
      = 1 + 1 + 1 + length []
      = 1 + 1 + 1 + 0
      = 3

        drop 3 [1,2,3,4,5]
      = drop 2 [2,3,4,5]
      = drop 1 [3,4,5]
      = drop 0 [4,5]
      = [4,5]

        init [1,2,3]
      = 1:init [2,3]
      = 1:2:init[3]
      = 1:2:[]
      = [1,2] 
-}


-- 3. Define library functions
and' :: [Bool] -> Bool
and' []         = True
and' (False:_)  = False
and' (_:xs)     = and' xs

concat' :: [[a]] -> [a]
concat' []      = []
concat' (x:xs)  = x ++ concat' xs

replicate' :: Int -> a -> [a]
replicate' 0 x  = []
replicate' n x  = x : replicate' (n - 1) x 

-- select nth element from list, counting from zero:
(!!!) :: [a] -> Int -> a
[] !!! n         = error "index too large"
_  !!! n | n < 0 = error "negative index"
(x:xs) !!! 0     = x
(x:xs) !!! n     = xs !!! (n - 1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ []              = False
elem' e (x:xs) | e == x = True
elem' e (x:xs)          = elem' e xs


-- 4. Merging two sorted lists
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys             = ys
merge xs []             = xs
merge (x:xs) (y:ys)
        | x < y         = x : merge xs     (y:ys)
        | otherwise     = y : merge (x:xs) ys


-- 5. Mergesort
msort :: Ord a => [a] -> [a]
msort []        = []
msort [a]       = [a]
msort xs        = merge (msort a) (msort b)
        where (a,b) = halve xs

halve :: [a] -> ([a], [a])
halve xs = (take cutoff xs, drop cutoff xs)
        where cutoff = length xs `div` 2 
{-
   Note: the signature in the textbook is:
              halve :: [a] -> [([a], [a])]
   I changed it since I saw no benefit in putting the result of the
   function in a list.
-}


-- 6. Redefining library functions
sum' :: Num a => [a] -> a
sum' []         = 0
sum' (x:xs)     = x + sum' xs

take' :: Int -> [a] -> [a]
take' n xs | n < 0 = error "negative index"
take' 0 xs         = []
take' n []         = []
take' n (x:xs)     = x : take' (n - 1) xs

last' :: [a] -> a
last' []        = error "empty list"
last' [a]       = a
last' (x:xs)    = last' xs
