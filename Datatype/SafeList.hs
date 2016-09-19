module SafeList where
  safeInit :: [a] -> Either String [a]
  safeInit [] = Left "Empty list exception in init"
  safeInit l = Right $ init l

  safeTail :: [a] -> Either String [a]
  safeTail [] = Left "Empty list exception in tail"
  safeTail l = Right $ tail l

  strip' :: [a] -> [a]
  strip' l = case safeInit l >>= safeTail of
              Left a -> []
              Right a -> a


  strip :: [a] -> Either String [a]
  strip l = safeInit l >>= safeTail