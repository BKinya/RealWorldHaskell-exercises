import Data.Char(toUpper)

main :: IO ()
main = do
  inpStr <- readFile "input.txt"
  writeFile "output1.txt" (map toUpper inpStr)
