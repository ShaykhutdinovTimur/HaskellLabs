module ListList where

    import Data.List

    zipN :: ([a] -> b) -> [[a]] -> [b]
    zipN f = map f . transpose

    transpose' :: [[a]] -> [[a]]
    transpose' [[]]    = []
    transpose' [[], _] = []
    transpose' rows    = (map head rows) : transpose' (map tail rows)

    test1 = take 5 $ zipN (take 3) $ repeat [0..1]
    test2 = take 5 $ zipN (take 3) $ repeat [0..]
