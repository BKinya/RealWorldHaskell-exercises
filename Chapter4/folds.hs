-- A function that takes a string of decimal digits and then turns them into an Integer

import Data.Char (digitToInt)
import Data.List
import Data.Function (on)
import Data.Char (toLower)


asInt :: String -> Int
loop :: Int -> String -> Int

asInt xs = loop 0 xs

loop acc [] = acc 
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
  in loop acc' xs


-- rewrite asInt function using a fold operation

asInt_fold :: String -> Int
asInt_fold ""                  = 0
asInt_fold (x:xs) | x == '-'   = (-1) * asInt_fold xs
                  | otherwise  = foldl' charToInt 0 (x:xs)
  where charToInt :: Int -> Char -> Int
        charToInt acc x = acc * 10 + digitToInt x

{-
Implementing the above function with foldr reverse the digits,
i.e "123" will become 321 but -124
-}

asInt_rightFold :: String -> Int
asInt_rightFold ""                  = 0
asInt_rightFold (x:xs) | x == '-'   = (-1) * asInt_rightFold xs
                  | otherwise  = foldr charToInt 0 (x:xs)
  where charToInt :: Char -> Int -> Int
        charToInt x acc  = acc * 10 + digitToInt x



{-
 The Prelude function concat concatenates a list of lists into a single list and has thefollowing type:
                concat :: [[a]] -> [a]

 Write your own definition of concat using foldr
-}

myConcat :: [[a]] -> [a]
myConcat [] = []

myConcat xss = foldr (++) [] xss




--  Write your own definition of the standard takeWhile function, first using explicitrecursion, and then foldr.
--  Using recursion

myTakeWhile :: (a->Bool) -> [a] -> [a]
myTakeWhile f (x:xs)
  | f x       = x: myTakeWhile f xs
  | otherwise = myTakeWhile f xs


myTakeWhile _ _ = []


-- Using foldr

niceTakeWhile :: (a->Bool) -> [a] -> [a]
niceTakeWhile f xs = foldr step [] xs
  where step x ys
          | f x       = x: ys
          | otherwise = ys


{-
Use ghci to load the Data.List module and figure out what groupBy does, thenwrite your own implementation using a fold.
-}

{-
 This code not working correctly
 groupByOddEven [1, 2, 3, 4, 5] returns [[1], [2], [3], [4], []5]] instead of
 [[1, 3, 5], [2, 4]]
-}

myGroupBy :: (a -> a -> Bool)-> [a] -> [[a]]
myGroupBy _ [] = []
myGroupBy f (x:xs) = (x:ys) : myGroupBy f zs
  where (ys, zs) = span (f x) xs


result xs = myGroupBy (\x y-> head x == head y) xs

sameParity :: Int -> Int -> Bool
sameParity x y = (odd x) == odd y

groupByOddEven xs = myGroupBy sameParity xs

