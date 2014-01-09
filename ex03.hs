{-
   "Graham Hutton: Programming in Haskell"
   Chapter 3: Solutions to exercises

   Gregor Ulm
-}


-- 1. Determining types of values
{-
['a', 'b', 'c']
[Char]

('a', 'b', 'c')
(Char, Char, Char)

[(False, 'O'), (True, '1')]
[(Bool, Char)]

([False, True], ['0', '1'])
([Bool], [Char])

[tail, init, reverse]
[[a] -> [a]]
-}


-- 2. Determining types of functions
second :: [a] -> a
second xs       = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x, y)     = (y, x)

pair :: a -> b -> (a, b)
pair x y        = (x, y)

double :: (Num a) => a -> a
double x        = x * 2

palindrome :: (Eq a) => [a] -> Bool
palindrome xs   = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x       = f (f x)


-- 3. Checking solutions in the interpreter
{-
   Hint: type signatures of anonymous functions can be checked
         the following way:
                :t \x -> 2*x
-}


-- 4.
{-
This is only feasible if there is a small number of argument values.
-}

