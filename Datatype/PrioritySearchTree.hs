module PrioritySearchTree where
    import Data.List

    data PST a = Nil | Node {x :: a, y :: a, miny :: a, maxy :: a, left :: PST a, right :: PST a} deriving (Show, Eq)

    buildPST :: (Ord a) => [(a, a)] -> PST a
    buildPST [] = Nil
    buildPST list = let
        r@(rootx, rooty) = minimum list
        list' = sort $ delete r list
        len = length list'
        (downList, topList) = splitAt (div len 2) list'
        down = buildPST downList
        top = buildPST topList
        min_y = if down /= Nil then min (miny down) rooty else rooty
        max_y = if top /= Nil then max (maxy top) rooty else rooty
        in Node {x = rootx, y = rooty, miny = min_y, maxy = max_y, left = down, right = top}

    req :: (Ord a) => PST a -> a -> a -> a -> [(a, a)]
    req Nil _ _ _ = []
    req root x0 y1 y2 = if (x root > x0 || miny root > y2 || maxy root < y1) then []
        else if (y1 <= y root && y root <= y2) then
            (x root, y root) : ((req (left root) x0 y1 y2) ++ (req (right root) x0 y1 y2))
            else (req (left root) x0 y1 y2) ++ (req (right root) x0 y1 y2)
