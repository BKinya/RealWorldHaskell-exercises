import Data.Char
import Data.Map (Map)
import qualified Data.Map as M
import Data.List




-- Determine if a year is a leap year

isLeapYear :: Integer -> Bool
isLeapYear year
  | year `mod` 4 == 0 && year `mod` 100 /= 0    = True
  | year `mod` 100 == 0 && year `mod` 400  == 0 = True
  | otherwise                                   = False


{-
Write a function that returns the earned points in a single toss of a Darts game.

Rules:
- If the dart lands outside the target, player earns no points (0 points).
- If the dart lands in the outer circle of the target, player earns 1 point.
- If the dart lands in the middle circle of the target, player earns 5 points.
- If the dart lands in the inner circle of the target, player earns 10 points.

The outer circle has a radius of 10 units (this is equivalent to the total radius for the entire target), the middle circle a radius of 5 units, and the inner circle a radius of 1. Of course, they are all centered at the same point — that is, the circles are concentric defined by the coordinates (0, 0).

-}

score :: Float -> Float -> Int
score x y
  | distance <= 1  = 10
  | distance <= 5  = 5
  | distance <= 10 = 1
  | otherwise     = 0
  where distance = sqrt (x^2 + y^2)


{-
Given an age in seconds, calculate how old someone would be on:

Mercury: orbital period 0.2408467 Earth years
Venus: orbital period 0.61519726 Earth years
Earth: orbital period 1.0 Earth years, 365.25 Earth days, or 31557600 seconds
Mars: orbital period 1.8808158 Earth years
Jupiter: orbital period 11.862615 Earth years
Saturn: orbital period 29.447498 Earth years
Uranus: orbital period 84.016846 Earth years
Neptune: orbital period 164.79132 Earth years

So if you were told someone were 1,000,000,000 seconds old, you should be able to say that they're 31.69 Earth-years old.

-}
data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds = case planet of
  Mercury  -> earthYears seconds / 0.2408467
  Venus    -> earthYears seconds / 0.61519726
  Earth    -> earthYears seconds / 1.0
  Mars     -> earthYears seconds / 1.8808158
  Jupiter  -> earthYears seconds / 11.862615
  Saturn   -> earthYears seconds / 29.447498
  Uranus   -> earthYears seconds / 84.016846 
  Neptune  -> earthYears seconds / 164.79132 
 where earthYears :: Float -> Float
       earthYears seconds = seconds / 31557600 -- 31557600 is equivalent to 1.0 earth year


-- Determine if a string is pangram

isPangram :: String -> Bool
isPangram "" = False
isPangram text = all (`elem` map toLower text) ['a' .. 'z']


-- Bob
isNotEmpty :: String -> Bool
isNotEmpty [] = False
isNotEmpty _  = True

responseFor :: String -> String
responseFor xs
  | isSilence xs             = "Fine. Be that way!"
  | isAsking  && isYell      = "Calm down, I know what I'm doing!"
  | isYell                   = "Whoa, chill out!"
  | isAsking                 = "Sure."
  | otherwise                = "Whatever."
  where
    text      = filter isLetter xs
    isYell    = isNotEmpty text && all isUpper text
    isSilence = all isSpace
    isAsking  = last (filter (/= ' ') xs) == '?'


{-

Collatz Conjecture
The Collatz Conjecture or 3x+1 problem can be summarized as follows:

Take any positive integer n. If n is even, divide n by 2 to get n / 2. If n is odd, multiply n by 3 and add 1 to get 3n + 1. Repeat the process indefinitely. The conjecture states that no matter which number you start with, you will always reach 1 eventually.

Given a number n, return the number of steps required to reach 1.
-}


collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0          = Nothing
  | n == 1          = Just 0
  | even n          = succ <$> collatz (n `div` 2)
  | odd n           = succ <$> collatz (3 * n + 1)


-- A tail rec version
collatz' :: Integer -> Integer -> Maybe Integer
collatz' n acc
  | n <= 0    = Nothing
  | n == 1    = Just acc
  | even n    = collatz' (n `div` 2) (acc + 1)
  | odd n     = collatz' (3 * n + 1) (acc + 1)


-- RNA transcription

toRNA :: String -> Either Char String
toRNA [] = Right []
toRNA (x:xs)
  | x == 'G'  = ('C': ) <$> toRNA xs
  | x == 'C'  = ('G': ) <$> toRNA xs
  | x == 'T'  = ('A': ) <$> toRNA xs
  | x == 'A'  = ('U': ) <$> toRNA xs
  | otherwise = Left x
          

-- Nucleotide Count
data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

parseNucleotide :: String -> Maybe Nucleotide
parseNucleotide "A" = Just A
parseNucleotide "C" = Just C
parseNucleotide "G" = Just G
parseNucleotide "T" = Just T
parseNucleotide _ = Nothing

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts [] = Right (M.fromList [(A, 0), (C, 0), (G, 0), (T, 0)])
nucleotideCounts xs
  | isValid xs = Right $ count xs
  | otherwise = Left xs
  where isValid = all (`elem` "ACGT")


count :: String -> Map Nucleotide Int
count xs = M.fromListWith (+) [(nucleotide, 1) | c <- xs, Just nucleotide <- [parseNucleotide [toUpper c]]]


{-
Introduction
You work for a company that makes an online, fantasy-survival game.

When a player finishes a level, they are awarded energy points. The amount of energy awarded depends on which magical items the player found while exploring that level.

Instructions
Your task is to write the code that calculates the energy points that get awarded to players when they complete a level.

The points awarded depend on two things:

- The level (a number) that the player completed.
- The base value of each magical item collected by the player during that level.

The energy points are awarded according to the following rules:

1. For each magical item, take the base value and find all the multiples of that value that are less than the level number.
2. Combine the sets of numbers.
3. Remove any duplicates.
4. Calculate the sum of all the numbers that are left.

Let's look at an example:

The player completed level 20 and found two magical items with base values of 3 and 5.

To calculate the energy points earned by the player, we need to find all the unique multiples of these base values that are less than level 20.

- Multiples of 3 less than 20: {3, 6, 9, 12, 15, 18}
- Multiples of 5 less than 20: {5, 10, 15}
- Combine the sets and remove duplicates: {3, 5, 6, 9, 10, 12, 15, 18}
- Sum the unique multiples: 3 + 5 + 6 + 9 + 10 + 12 + 15 + 18 = 78
Therefore, the player earns 78 energy points for completing level 20 and finding the two magical items with base values of 3 and 5.
-}

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum .nub $ factors >>= takeWhile (< limit). multiples


multiples :: Integer -> [Integer]
multiples 0 = [0]
multiples n = [m * n | m <- [1..]]
