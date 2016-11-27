module PLTOS where

data Syscall = Read | Write | Exit | Yield | Fork

data Process = Cons Syscall Process | Nil

kernel :: (Show argv, Read argv) => [(Process, Maybe argv)] -> IO ([Int])
kernel [] = return []
kernel (p:ps) = case fst p of Nil             -> (-1:) <$> kernel ps
                              (Cons Read  p') -> readArg >>= \arg -> kernel ((p', Just arg):ps)
                              (Cons Write p') -> writeArg (snd p) >> kernel ((p', Nothing):ps)
                              (Cons Exit  p') -> (0:) <$> kernel ((p', Nothing):ps)
                              (Cons Yield p') -> kernel ((p', Nothing):ps)
                              (Cons Fork  p') -> kernel ((p', snd p):(p', snd p):ps)

readArg :: (Show a, Read a) => IO (a)
readArg = readLn

writeArg :: (Show a) => Maybe a -> IO ()
writeArg Nothing = print ""
writeArg (Just a) = print a


