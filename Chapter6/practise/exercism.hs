{- Implement the keep and discard operation on collections. Given a collection and a predicate on the collection's elements, keep returns a new collection containing those elements where the predicate is true, while discard returns a new collection containing those elements where the predicate is false.
-}

discard :: (a -> Bool) -> [a] -> [a]
discard p (x:xs)
  | not (p x)  =  x : discard p xs
  | otherwise  = discard p xs
discard p _ = []


keep :: (a -> Bool) -> [a] -> [a]
keep p (x:xs)
  | p x        = x: keep p xs
  | otherwise  =  keep p xs

keep p _ = []
