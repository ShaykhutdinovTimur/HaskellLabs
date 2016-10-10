{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Set where

import TreePrinters
import BinarySearchTree as BST hiding (lowest)
import Data.Foldable    as DF

class Set t a where
    emptySet :: t a
    toList   :: t a -> [a]
    find     :: a -> t a -> t a
    insert   :: a -> t a -> t a
    delete   :: a -> t a -> t a
    next     :: a -> t a -> Maybe a
    fromList :: [a] -> t a

lowest :: (Ord a) => Tree a -> Maybe a
lowest Leaf = Nothing
lowest (Node x Leaf _) = Just x
lowest (Node _ l _) = lowest l

instance (Ord a) => Set Tree a where
    emptySet        = mempty
    toList          = DF.toList
    find            = BST.find
    insert          = BST.insert
    delete          = BST.delete
    next _ Leaf     = Nothing
    next k (Node x l r)
        | k >= x    = lowest r
        | otherwise = if isNothing (next k l) then Nothing
                        else Just x

    fromList        = BST.fromList



