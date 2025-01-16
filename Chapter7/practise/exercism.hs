import Data.List (sort)
import Data.Char (toLower)
import Data.Char (isDigit)
import Data.List (reverse)

anagramsFor :: String -> [String] -> [String]
anagramsFor target (x:xs)
  | (map toLower target)        == (map toLower x)        = anagramsFor target xs
  | (sort (map toLower target)) == (sort (map toLower x)) = x : anagramsFor target xs
  | otherwise                                             = anagramsFor target xs


anagramsFor target _                                      = []


---

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0   = Nothing
  | mSum == n = Just Perfect
  | mSum > n  = Just Abundant
  | mSum < n  = Just Deficient
  | otherwise =  Nothing

  where mSum = sum (factors n) 


factors n = [x | x <- [1..(n-1)], n `mod` x == 0]


---

isValid :: String -> Bool
isValid n
  | (length (removeWhiteSpaces n)<= 1)     = False
  | not ( all isDigit (filter (/= ' ') n)) = False
  | sum `mod` 10 == 0                      = True 
  | otherwise                              = False
  
  where sum = helper n


helper :: String -> Int
helper  = sum . doubleEverySecondNumber . reverse . stringToDigitList . removeWhiteSpaces


removeWhiteSpaces :: String -> String
removeWhiteSpaces = filter (/= ' ')

doubleEverySecondNumber :: [Int] -> [Int]
doubleEverySecondNumber (x:y:xs) =  x : ( sumDigits(2 * y)) : doubleEverySecondNumber xs
doubleEverySecondNumber a = a

sumDigits :: Int -> Int
sumDigits n
  | n == 0 = 0
  | n > 0 && n < 10 = n
  | otherwise = (mod n 10) + sumDigits (div n 10)

stringToDigitList :: String -> [Int]
stringToDigitList [] = []
stringToDigitList (x:xs) 
  | isDigit x = (read [x] :: Int) : stringToDigitList xs
  | otherwise = stringToDigitList xs


number :: String -> Maybe String
number [] = Nothing
number xs
  | length cleaned == 10 = if ((&& isValidAreaCode cleaned) $ isValidExchangeCode cleaned ) then Just cleaned else Nothing
  | length cleaned == 11 = if(isValidCountryCode && (isValidAreaCode $ tail cleaned) && (isValidExchangeCode $ tail cleaned)) then Just (tail cleaned) else Nothing
  | otherwise = Nothing
  where cleaned = filter isDigit xs
        isValidCountryCode = (head cleaned) == '1'
        isValidAreaCode n = (read[head n] :: Int) `elem` [2..9]
        isValidExchangeCode m = (read [(m !! 3)] :: Int) `elem` [2..9]

------

recite :: Int -> Int -> [String]
recite start stop = reciteWithAcc start stop []


reciteWithAcc :: Int -> Int -> [String] -> [String]
reciteWithAcc start stop acc  =
  if start > stop
  then acc
  else reciteWithAcc start' stop acc'
  where start' = start + 1
        acc' =  acc ++ [someNew]
        someNew =  case start of
          1 -> "On the first day of Christmas my true love gave to me: a Partridge in a Pear Tree."
          2 -> "On the second day of Christmas my true love gave to me: two Turtle Doves, and a Partridge in a Pear Tree."
          3 -> "On the third day of Christmas my true love gave to me: three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
          4 -> "On the fourth day of Christmas my true love gave to me: four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
          5 -> "On the fifth day of Christmas my true love gave to me: five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
          6 -> "On the sixth day of Christmas my true love gave to me: six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
          7 -> "On the seventh day of Christmas my true love gave to me: seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
          8 -> "On the eighth day of Christmas my true love gave to me: eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
          9 -> "On the ninth day of Christmas my true love gave to me: nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
          10 -> "On the tenth day of Christmas my true love gave to me: ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
          11 -> "On the eleventh day of Christmas my true love gave to me: eleven Pipers Piping, ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
          12 -> "On the twelfth day of Christmas my true love gave to me: twelve Drummers Drumming, eleven Pipers Piping, ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
          _ -> "Out of bounds"


-- An alternate solution for the above
recite' :: Int -> Int -> [String]
recite' start stop =  map verse [start..stop]

verse :: Int -> String
verse n = case n of
  1 -> "On the first day of Christmas my true love gave to me: a Partridge in a Pear Tree."
  2 -> "On the second day of Christmas my true love gave to me: two Turtle Doves, and a Partridge in a Pear Tree."
  3 -> "On the third day of Christmas my true love gave to me: three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
  4 -> "On the fourth day of Christmas my true love gave to me: four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
  5 -> "On the fifth day of Christmas my true love gave to me: five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
  6 -> "On the sixth day of Christmas my true love gave to me: six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
  7 -> "On the seventh day of Christmas my true love gave to me: seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
  8 -> "On the eighth day of Christmas my true love gave to me: eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
  9 -> "On the ninth day of Christmas my true love gave to me: nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
  10 -> "On the tenth day of Christmas my true love gave to me: ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
  11 -> "On the eleventh day of Christmas my true love gave to me: eleven Pipers Piping, ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
  12 -> "On the twelfth day of Christmas my true love gave to me: twelve Drummers Drumming, eleven Pipers Piping, ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
  _ -> "Out of bounds"
