module KListElem where
    kListElem :: Int -> [a] -> [a]
    kListElem k list = if (k > length list) then list else let
        (begining, _:ending) = splitAt (k - 1) list
        in begining ++ kListElem k ending

