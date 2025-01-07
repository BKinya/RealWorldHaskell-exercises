import JSONClass

-- Turn a list into what JSON calls an array
instance (JSON a) => JSON [a] where
  toJValue = undefined
  fromJValue = undefined



-- turn a list of name/value pairs into a JSOn
instance (JSON a) => JSON [(String, a)] where
  toJValue = undefined
  fromJValue = undefined
