--whileM реализовать без MonadPlus
module WhileM where

whileM :: Monad m => m Bool -> m a -> m [a]
whileM a b = a >>= \cond -> if cond then b >>= \res -> ((return (res :)) <*> (whileM a b))
                                    else return []
