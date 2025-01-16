main :: IO ()
main  = do
  putStrLn " Hola! Como te llamas?"
  inpStr <- getLine
  putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"
