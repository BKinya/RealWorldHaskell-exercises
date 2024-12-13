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
