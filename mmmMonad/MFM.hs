{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module MFM where

import FishJoin

instance MonadFish m => Monad m where
    return = returnFish
    x >>= f = ((\x -> x) >=> f) x