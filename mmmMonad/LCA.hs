module LCA where

import Data.Maybe

data InNode a = Node { label :: a, parent :: Maybe (InNode a) } deriving (Eq, Show)

getParents :: InNode a -> [InNode a]
getParents x 
    | isNothing (parent x) = []
    | otherwise = par : (getParents par) where
        Just par = parent x

leastCommonAncestor :: Eq a => InNode a -> InNode a -> Maybe (InNode a)
leastCommonAncestor x y = fstCommon (x : getParents x) (y : getParents y)

fstCommon :: Eq a => [a] -> [a] -> Maybe a
fstCommon [] _ = Nothing
fstCommon (x:xs) y = if ((filter (==x) y) /=  []) then Just x else fstCommon xs y



