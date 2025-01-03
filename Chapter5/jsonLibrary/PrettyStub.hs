import SimpleJson


data Doc = ToBeDefined
          deriving (Show)


string :: String -> Doc
string str = undefined

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined

-- (<>) appends two doc values
-- It's the Doc equivalent of (++)
(<>) :: Doc -> Doc -> Doc
a <> b = undefined


char :: Char -> Doc
char c = undefined

-- concatenates multiple Doc values into one
-- analogue of concat in list
hcat :: [Doc] -> Doc
hcat xs = undefined

fsep :: [Doc] -> Doc
fsep xs = undefined


