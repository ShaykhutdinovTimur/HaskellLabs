{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module FishJoin where

import Data.List
import Data.Function

class MonadFish m where
    returnFish :: a -> m a
    (>=>)      :: (a -> m b) -> (b -> m c) -> (a -> m c)

class MonadJoin m where
    returnJoin :: a -> m a
    join       :: m (m a) -> m a

class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

instance MonadJoin [] where
    returnJoin x = [x]
    join = concat

instance MonadFish [] where
    returnFish x = [x]
    f >=> g = concat . map g . concat . map f . returnFish