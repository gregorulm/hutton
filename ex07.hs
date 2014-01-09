{-
   "Graham Hutton: Programming in Haskell"
   Chapter 7: Solutions to exercises

   Gregor Ulm
-}

import Test.QuickCheck
import Text.Show.Functions
import Data.Char

-- Exercises for Chapter 07

--1.
{-
        The list comprehension [f x | x <- xs, p x] can be expressed
        using map and filter as follows:
                map f (filter p xs)
-}
double :: Int -> Int
double x = 2 * x

testList :: [Int] -> (Int -> Bool) -> (Int -> Int) -> [Int]
testList xs p f        = [f x | x <- xs, p x]

testMapFilter :: [Int] -> (Int -> Bool) -> (Int -> Int) -> [Int]
testMapFilter xs p f  = map f (filter p xs)

-- simple check
prop1 = testList xs p f == testMapFilter xs p f
        where xs = [1,2,3,4]
              p = even
              f = double

-- QuickCheck property
prop :: [Int] -> (Int -> Bool) -> (Int -> Int) -> Bool
prop xs p f = testList xs p f == testMapFilter xs p f


-- 2. Redefining library functions
all' :: (a -> Bool) -> [a] -> Bool
all' p xs = and (map p xs)

any' :: (a -> Bool) -> [a] -> Bool
any' p xs = or (map p xs)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p []      = []
takeWhile' p (x:xs)  | p x       = x : takeWhile' p xs
                     | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p []      = []
dropWhile' p (x:xs)  | p x       = dropWhile' p xs
                     | otherwise = x:xs


-- 3. Redefining map and filter

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\y ys -> f y:ys) []  

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\y ys -> if p y then y:ys else ys) []


-- 4. dec2int
dec2int :: [Int] -> Int
dec2int xs = foldl (\x xs -> x * 10 + xs) 0 xs 


-- 5.
{-
        The following definition is invalid since the function  
        "compose" has not been defined.
        sumsqreven = compose [sum, map (^2), filter even]

        Function composition looks like this in Haskell:
        sumsqreven = sum . map (^2) . filter even
-}


-- 6.
--curry' :: ((Int, Int) -> Int) -> Int -> Int -> Int
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f ps    = f (fst ps) (snd ps)  


-- 7. Unfold

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

int2bin' :: Int -> [Bit]
int2bin' = unfold (==0) (`mod` 2) (`div` 2)

chop8' :: [Bit] -> [[Bit]]
chop8' = unfold (== []) (take 8) (drop 8)

map'' :: Eq a => (a -> b) -> [a] -> [b]
map'' f = unfold (==[]) (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' = unfold (const False) id 
--iterate' (*2) 1


-- 8. Detect transmission errors

{-
-- String transmission code from the book:

type Bit = Int

bin2int :: [Bit] -> Int
--bin2int bits = sum [ w * b | (w,b) <- zip weights bits ]
--        where weights = iterate (*2) 1
bin2int = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)
--encode "abc"

chop8 :: [Bit] -> [[Bit]]
chop8 []   = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8
--decode [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]
--"abc"

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id
-}

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concatMap (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 []   = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

-- adds parity bit to every [Bit]
addParity :: [[Bit]] -> [[Bit]]
--addParity = map (\xs -> if odd (sum xs) then xs ++ [1] else xs ++ [0])
addParity = map (\xs -> (++) xs (if odd (sum xs) then [1] else [0]))


encodeParity :: String -> [[Bit]]
encodeParity = addParity . chop8 . encode
-- encodeParity "abc"
--[[1,0,0,0,0,1,1,0,1],[0,1,0,0,0,1,1,0,1],[1,1,0,0,0,1,1,0,0]]


decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

decodeParity :: [[Bit]] -> String
decodeParity = concatMap checkBit
        where parityOK xs = odd (sum (init xs)) && last xs == 1
                            || even (sum (init xs)) && last xs == 0
              checkBit xs = if parityOK xs
                            then decode (init xs)
                            else error "parity error"

-- decodeParity [[1,0,0,0,0,1,1,0,1],[0,1,0,0,0,1,1,0,1],[1,1,0,0,0,1,1,0,0]]
-- "abc"

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

transmit' :: String -> String
transmit' = decodeParity . channel' . encodeParity

channel' :: [[Bit]] -> [[Bit]]
channel' = id


-- 9. Modelling a faulty communication

transmit'' :: String -> String
transmit'' = decodeParity . channel'' . encodeParity

channel'' :: [[Bit]] -> [[Bit]]
channel'' = map tail
-- transmit'' "abc"
-- "*** Exception: parity error
