module Expressions where

import Control.Monad.Reader

type VarName = String
type LiteralType = Int

data Expression = Var VarName
                | Lit LiteralType
                | Mul Expression Expression
                | Add Expression Expression
                | Assign VarName LiteralType Expression deriving (Show, Eq)

evaluate :: Expression -> Reader VarName LiteralType -> LiteralType
evaluate (Var v) ctx = runReader ctx v
evaluate (Lit n) _ = n
evaluate (Mul l r) ctx = (evaluate l ctx) * (evaluate r ctx)
evaluate (Add l r) ctx = (evaluate l ctx) + (evaluate r ctx)
evaluate (Assign v c e) ctx = evaluate e $ combined v c ctx

combined :: Eq a => a -> b -> Reader a b -> Reader a b
combined k v r = reader $ \x -> if x == k then v else runReader r k

--tests
testExpr :: Expression
testExpr = Var "x" `Add` (Lit 3 `Mul` ("x" `Assign` 2 $ Var "x"))

myMap :: VarName -> LiteralType
myMap "x" = 1
myMap _ = 0
