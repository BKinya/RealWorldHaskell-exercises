module DataEither where

data MMaybe a = Nothing
  | Just a
  deriving (Eq, Ord, Read, Show)

data MEither a b = Left a
  | Right b
  deriving (Eq, Ord, Read, Show)
