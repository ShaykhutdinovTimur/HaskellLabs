module SMA where

import Control.Monad.State

moving :: (Fractional a) => Int -> [a] -> [a]
moving n s = sma ([], s, n)

smaStep :: (Fractional a) => State ([a], [a], Int) a
smaStep = state $ \(d, p, num) -> let
    d' = if (length d < num) then head p : d
                             else init (head p : d)
    p' = tail p
    ans = (sum d') / (fromIntegral (length d'))
    in (ans, (d', p', num))

sma :: (Fractional a) => ([a], [a], Int) -> [a]
sma (_, [], _) = []
sma x = let
    (h, res) = runState smaStep x
    t = sma res
    in h : t