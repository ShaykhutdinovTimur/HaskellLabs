{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module MFMJ where

import FishJoin

instance MonadFish m => MonadJoin m where
    returnJoin = returnFish
    join = (\x -> x) >=> (\x -> x)

