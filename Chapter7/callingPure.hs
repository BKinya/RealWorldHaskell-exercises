name2reply :: String -> String
name2reply name =
  "Mucho gusto, " ++ name ++ ".\n" ++
  "Your name contains " ++ charCount ++ " characters."
  where charCount = show (length name)



main :: IO ()
main = do
  putStrLn "Hola! Como te llamas?"
  inpStr <- getLine
  let outStr = name2reply inpStr
  putStrLn outStr
        
