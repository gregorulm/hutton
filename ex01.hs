{-
   "Graham Hutton: Programming in Haskell"
   Chapter 1: Solutions to exercises

   Gregor Ulm
-}


-- 1. Possible calculation for the result of "double (double 2)" 
{-
  double (double 2)
= double (2 + 2)
= double 2 + double 2
= 4 + 4
= 8
-}


-- 2. Show that sum [x] = x for any number x
{-
  sum [x]
= sum (x:xs)
= x + sum xs
= x + sum []
= x + 0
= x
-}


-- 3. Product of a list of numbers
product' :: [Integer] -> Integer
product' []     = 1
product' (x:xs) = x * product' xs

{-
  product' [2,3,4]
= 2 * product' [3,4]
= 2 * 3 * product' [4]
= 2 * 3 * 4 * product' []
= 2 * 3 * 4 * 1
= 24
-}


-- 4. Reverse quick sort
qsortR :: [Integer] -> [Integer]
qsortR []     = []
qsortR (x:xs) = qsortR hi ++ [x] ++ qsortR lo
        where hi = [a | a <- xs, a >= x ]
              lo = [a | a <- xs, a < x ]


-- 5. Importance of <= in the definition of qsort
{-
   Equality sign is necessary in case there are duplicates in the list.
   Replacing "<=" with "<" means that duplicates will be removed.
-}
qsort :: [Integer] -> [Integer]
qsort []     = []
qsort (x:xs) = qsort lo ++ [x] ++ qsort hi
	where lo = [a | a <- xs, a < x]
              hi = [a | a <- xs, a > x]
	

