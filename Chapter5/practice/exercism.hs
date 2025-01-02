import Data.Char

{- Calculate the number of grains of wheat on a chessboard given that the number on each square doubles.

There once was a wise servant who saved the life of a prince. The king promised to pay whatever the servant could dream up. Knowing that the king loved chess, the servant told the king he would like to have grains of wheat. One grain on the first square of a chess board, with the number of grains doubling on each successive square.

There are 64 squares on a chessboard (where square 1 has one grain, square 2 has two grains, and so on).

Write code that shows:

how many grains were on a given square, and
the total number of grains on the chessboard
-}

square :: Integer -> Maybe Integer
square n
  | (n > 0 ) && (n <= 64 ) = Just (2 ^ (n - 1))
  | otherwise              = Nothing

total :: Integer
total = totalRecursive 1 0


totalRecursive :: Integer -> Integer-> Integer
totalRecursive start acc
  | start > end = acc
  | otherwise = totalRecursive start' acc'
  where end = 64
        acc' = acc + (2^(start - 1))
        start' = start + 1

{---- I do not need the totalRecursive function. I could easily calculate total
like this total = 2^64 - 1
--}
   


{- Write a function splitWith that acts similarly to words but takes a predicate and a
list of any type, and then splits its input list on every element for which the predicate
returns False:
-}
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f xs  =
  let (prefix, suffix)  = break (not . f) xs
  in if null prefix
     then []
     else prefix : splitWith f  (dropWhile (not . f) suffix)
          

splitWith' :: (a -> Bool) -> [a] -> [[a]]
splitWith' f xs  =
  let (prefix, suffix)  = span f  xs
  in if null prefix
     then []
     else prefix : splitWith f  (dropWhile (not . f) suffix)


-- acronym
{-
 Does not produe the correct result for "HyperText Markup Language" input. The function returns
"HML" instead of "HTML"
-}

splitWith'' :: (Char -> Bool) -> [Char] -> [[Char]]
splitWith'' f xs  =
  let (prefix, suffix)  = break f  xs
  in if null prefix
     then []
     else prefix : splitWith'' f (dropWhile (\x -> f x || not (isAlpha x)) suffix)  

splitString :: String -> [String]
splitString xs =  splitWith'' f xs
  where f :: Char -> Bool
        f c  = isSpace c || c == '-' 

acronym :: [String] -> String
acronym [] = ""
acronym (x:xs) = toUpper (head x) : acronym xs


abbreviate :: String -> String
abbreviate xs = acronym (splitString xs)


-- Spilt a Camelcase word into several words
-- CamelCaseWord becomes ["Camel", "Case", "Word"]
splitAtUpper :: String -> Maybe (String, String)
splitAtUpper [] = Nothing
splitAtUpper (x:xs) = 
  case break isUpper xs of
    (before, after) -> Just (x:before, after)
    
splitCamelCase :: String -> [String]
splitCamelCase [] = []
splitCamelCase (x:xs) = 
  case splitAtUpper xs of
    Nothing -> [x:xs] 
    Just (before, after) -> [x:before] ++ splitCamelCase after




