isPrime :: Integer -> Bool
isPrime n
  | n <= 1 = False
  | n == 2 = True
  | otherwise = all (\x -> n `mod` x /= 0)[2 ..floor $ sqrt $ fromIntegral n]


nth :: Int -> Maybe Integer
nth n
  | n <= 0 = Nothing
  | otherwise =  Just (findNthPrimeNumber n 0 1)


findNthPrimeNumber :: Int -> Integer -> Integer -> Integer
findNthPrimeNumber 0 lastPrime _ = lastPrime
findNthPrimeNumber n lastPrime curNum
  | isPrime curNum = findNthPrimeNumber n' curNum curNum'
  | otherwise = findNthPrimeNumber n lastPrime curNum'
  where  n' = n - 1
         curNum' = curNum + 1


----
distance :: String -> String -> Maybe Int
distance xs ys
  | length xs /= length ys = Nothing
  | otherwise              = Just ( helper xs ys 0)


helper :: String -> String -> Int -> Int
helper "" "" count = count
helper (x:xs) (y:ys) count
  | x == y     = helper xs ys count
  | otherwise  = helper xs ys count'
  where count' = count + 1
