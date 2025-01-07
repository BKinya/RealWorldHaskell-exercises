{-# LANGUAGE FlexibleInstances #-}

import SimpleJson

class BasicEq a where
  isEqual ::  a -> a -> Bool

-- Defining isEqual for a particular type
-- Bool
instance BasicEq Bool where
  isEqual True  True  = True
  isEqual False False = True
  isEqual _     _     = False


class BasicEq2 a where
  isEqual2 :: a -> a -> Bool
  isNotEqual2 :: a -> a -> Bool



{-
Users implementing BasicEq3 must provide an
implementation of at least one function
-}

