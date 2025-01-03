module Main (main) where

import SimpleJson
import PutJson
main = putJValue (JObject [("foo", JNumber 1), ("bar", JBool False)])
 
