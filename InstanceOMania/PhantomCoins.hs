{-# LANGUAGE ScopedTypeVariables #-}

module PhantomCoins where

newtype Coin color = Coin { getCoin :: Int }

data Blue
data Red

blue = undefined :: Blue
red = undefined :: Red

createCoins :: color -> Int -> Coin color
createCoins _ = Coin

c1 = createCoins blue 10
c2 = Coin 5 :: Coin Red

addCoins :: Coin color -> Coin color -> Coin color
addCoins (Coin a) (Coin b) = Coin (a + b)

instance Num (Coin col) where
    (Coin x) + (Coin y) = Coin (x + y)
    (Coin x) * (Coin y) = Coin (x * y)
    (Coin x) - (Coin y) = Coin (x - y)
    negate (Coin x) = Coin (negate x)
    abs (Coin x) = Coin (abs x)
    signum (Coin x) = Coin (signum x)
    fromInteger x = Coin (fromInteger x)

instance Monoid (Coin col) where
    mempty = Coin 0
    mappend = addCoins

compare :: (Show col1, Show col2) => Coin col1 -> Coin col2 -> Ordering
compare (x :: Coin col1) (y :: Coin col2)
    | show (undefined :: col1) > show (undefined :: col2) = GT
    | show (undefined :: col1) < show (undefined :: col2) = LT
    | otherwise = Prelude.compare (getCoin x) (getCoin y)

instance Show Blue where
    show _ = "Blue"

instance Show Red where
    show _ = "Red"
