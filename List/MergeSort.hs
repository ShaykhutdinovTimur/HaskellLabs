module MergeSort where
    import System.Random (newStdGen, randomRs)

    randomIntList :: Int -> Int -> Int -> IO [Int]
    randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

    splitList :: [a] -> ([a], [a])
    splitList [] = ([],[])
    splitList [x] = ([x],[])
    splitList (x:y:zs) = (x:xs, y:ys) where
        (xs,ys) = splitList zs

    merge :: (Ord a) => [a] -> [a] -> [a]
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys) = if (x <= y) then x : merge xs (y:ys)
        else y : merge (x:xs) ys


    mergeSort :: (Ord a) => [a] -> [a]
    mergeSort [] = []
    mergeSort [x] = [x]
    mergeSort l = merge (mergeSort top) (mergeSort bottom) where
        (top, bottom) = splitList l


