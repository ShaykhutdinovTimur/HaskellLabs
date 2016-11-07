module SExpr where

import AParser
import Control.Applicative
import Data.Char

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = liftA2 (:) p (zeroOrMore p)


spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = liftA2 (:) (satisfy isAlpha) (zeroOrMore $ satisfy isAlphaNum)


type Ident = String

data Atom = N Integer
          | I Ident
  deriving Show

data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parseAtom:: Parser Atom
parseAtom = liftA N posInt
         <|> liftA I ident

parseSExpr:: Parser SExpr
parseSExpr = spaces *> sexpr <* spaces where
        sexpr = liftA A parseAtom
             <|> liftA Comb (char '(' *> zeroOrMore parseSExpr <* char ')')