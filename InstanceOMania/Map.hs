{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Map where

import TreePrinters
import BinarySearchTree as BST hiding (lowest)
import Data.Foldable    as DF
import Set

data MapEntry k v = MapEntry { key :: k, value :: v} deriving Show
instance (Eq k) => Eq (MapEntry k v) where
    x == y = key x == key y
instance (Ord k) => Ord (MapEntry k v) where
    x < y  = key x < key y
    x <= y = key x <= key y

class Map t k v where
    emptyMap :: t (MapEntry k v)
    toList   :: t (MapEntry k v) -> [MapEntry k v]
    find     :: k -> t (MapEntry k v) -> t (MapEntry k v)
    insert   :: MapEntry k v -> t (MapEntry k v) -> t (MapEntry k v)
    delete   :: k -> t (MapEntry k v) -> t (MapEntry k v)
    next     :: k -> t (MapEntry k v) -> Maybe (MapEntry k v)
    fromList :: [MapEntry k v] -> t (MapEntry k v)

instance (Ord k) => Map Tree k v where
    emptyMap = Set.emptySet
    toList   = Set.toList
    find k   = Set.find (MapEntry k undefined)
    insert   = Set.insert
    delete k = Set.delete (MapEntry k undefined)
    next k   = Set.next (MapEntry k undefined)
    fromList = Set.fromList
