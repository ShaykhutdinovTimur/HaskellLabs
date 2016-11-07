{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module State where

newtype State s r = State { fun :: s -> (s, r) }

instance (Applicative (State s)) => Monad (State s) where
    return = pure
    x >>= f = State $ \s -> let (s', res) = fun x s in fun (f res) s'
