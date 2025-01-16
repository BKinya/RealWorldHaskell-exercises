import Data.Char(toLower)

main :: IO ()
main = interact ( (++) "YOUR DATA, IN LOWER CASE IS : \n\n" .map toLower)
