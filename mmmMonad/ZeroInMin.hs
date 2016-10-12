module ZeroInMin where

manHeaps :: (Int, Int) -> [(Int, Int)]
manHeaps (a, b) = filter isCorrectHeaps
    [ (a - 1, b    ), (a   *   2, b `div` 2)
    , (a    , b - 1), (a `div` 2, b   *   2)
    ]
  where
    isCorrectHeaps (x, y) = x >= 0 && y >= 0

zeroInMin :: (Int, Int) -> Int
zeroInMin p@(x, y)
    | x == 0 && y == 0 = 0
    | otherwise = 1 + minimum (map (zeroInMin) ([p] >>= manHeaps >>= ))

