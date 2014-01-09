{-
   "Graham Hutton: Programming in Haskell"
   Chapter 4: Solutions to exercises

   Gregor Ulm
-}


-- 1. Halving a list
halve :: [a] -> ([a], [a])
halve xs = (take pos xs, drop pos xs)
        where pos = div (length xs) 2

-- 2. safetail
-- a. conditional expression
safetail1 :: [a] -> [a]
safetail1 xs = if null xs
               then []
               else tail xs

-- b. guarded equation
safetail2 :: [a] -> [a]
safetail2 xs | null xs   = []
             | otherwise = tail xs  

-- c. pattern matching
safetail3 :: [a] -> [a]
safetail3 []     = []
safetail3 (x:xs) = xs        


-- 3. Define logical 'or' operator
or1 :: Bool -> Bool -> Bool
or1 True True   = True
or1 True False  = True
or1 False True  = True
or1 False False = False

or2 :: Bool -> Bool -> Bool
or2 True _ = True
or2 _ True = True
or2 _ _    = False

or3 :: Bool -> Bool -> Bool
or3 False b = b
or3 True _  = True

or4 :: Bool -> Bool -> Bool
or4 a b | a == b = a
or4 _ _          = True

-- 4. Redefine conjunction operator
and2 :: Bool -> Bool -> Bool
and2 a b = if a && b
           then True
           else False

-- 5. dto.
and3 :: Bool -> Bool -> Bool
and3 a b = if a
           then b
           else if not a
               then False
               else error "shouldn't happen" -- required by Haskell

-- 6. "mult" expressed with an anonymous function
mult :: Integer -> Integer -> Integer -> Integer
---mult = \x -> (\y -> (\z -> x * y * z))
mult = \x y z -> x * y * z
