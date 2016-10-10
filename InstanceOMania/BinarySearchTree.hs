module BinarySearchTree where

import TreePrinters

find :: (Ord a) => a -> Tree a -> Tree a
find _ Leaf = Leaf
find k t@(Node x l r)
    | k == x = t
    | k < x = find k l
    | otherwise = find k r

insert :: (Ord a) => a -> Tree a -> Tree a
insert k Leaf = Node k Leaf Leaf
insert k (Node x l r)
    | k == x = Node x l r
    | k < x = Node x (insert k l) r
    | otherwise = Node x l (insert k r)

delete :: (Ord a) => a -> Tree a -> Tree a
delete _ Leaf = Leaf
delete k t@(Node x l r)
    | k > x = Node x l (delete k r)
    | k < x = Node x (delete k l) r
    | otherwise = deleteRoot t

deleteRoot :: (Ord a) => Tree a -> Tree a
deleteRoot (Node _ l Leaf) = l
deleteRoot (Node _ Leaf r) = r
deleteRoot (Node _ l r) = Node v l u where
    v = lowest r
    u = delete v r

lowest :: (Ord a) => Tree a -> a
lowest (Node x Leaf _) = x
lowest (Node _ l _) = lowest l

toList :: Tree a -> [a]
toList Leaf = []
toList (Node x l r) = x : (toList l ++ toList r)

fromList :: (Ord a) => [a] -> Tree a
fromList = foldr insert Leaf

instance Foldable Tree where
    foldr fun st tree = foldr fun st (toList tree)

instance (Ord a) => Monoid (Tree a) where
    mempty = Leaf
    mappend = foldr insert