module SimpleJson
  (
    -- A list of exports
    JValue (..) -- (..) indicates we are exporting both the type and all its constructors
  , getString
  , getInt
  , getDouble
  , getBool
  , getObject
  , getArray
  , isNull
  ) where -- keyword where indicates that the bod o the module follows

-- Use an Agebraic Data Type to represent the range of possible JSON types
data JValue = JString String
  | JNumber Double
  | JBool Bool
  | JNull
  | JObject [(String, JValue)]
  | JArray [JValue]
    deriving (Eq, Ord,Show)

{- Take a JValue and reverse it to a Normal Haskell value
 e.g JString to String
-}
getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing


getInt :: JValue -> Maybe Integer
getInt (JNumber n) = Just (truncate n)
getInt _           = Nothing

getDouble (JNumber n) = Just n
getDouble _           = Nothing


getBool (JBool b) = Just b
getBool _         = Nothing

getObject (JObject o) = Just o
getObject _           = Nothing


getArray (JArray a) = Just a
getArray  _         = Nothing


isNull v            = v == JNull
