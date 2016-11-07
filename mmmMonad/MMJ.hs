{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module MMJ where

import FishJoin

instance Monad m => MonadJoin m where
    returnJoin = return
    join x = x >>= \x -> x

