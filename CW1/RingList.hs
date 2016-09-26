module RingList where
    data RingList a = RingList { list :: [a] }

    shift :: RingList a -> RingList a
    shift (RingList a) = RingList (last a : init a)

    (!!!) :: RingList a -> Int -> a
    (!!!) rl x
        | x < 0 || x >= (length (list rl)) = error "ring list index out of bounds exception"
        | otherwise = list rl !! x