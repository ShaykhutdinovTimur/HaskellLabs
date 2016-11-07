{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module MMF where

import FishJoin

instance Monad m => MonadFish m where
    returnFish = return
    f >=> g = \x -> return x >>= f >>= g