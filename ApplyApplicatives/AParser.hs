module AParser where

import Control.Applicative
import Data.Char

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs


first :: (a -> b) -> (a, c) -> (b, c)
first f (res, str) = (f res, str)

instance Functor Parser where
        fmap f (Parser rp) = Parser (fmap (first f) . rp)

instance Applicative Parser where
        pure f = Parser (\str -> Just (f, str))
        p1 <*> p2 = Parser (\str -> case runParser p1 str of
                                Nothing -> Nothing
                                Just (f, x) -> first f
                                        <$> runParser p2 x)

abParser :: Parser (Char, Char)
abParser = (\ a b -> (a,b)) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = (\ _ _ ->  ()) <$> char 'a' <*> char 'b'

intPair:: Parser [Integer]
intPair = (\ x _ z -> [x,z]) <$> posInt <*> char ' ' <*> posInt

instance Alternative Parser where
        empty = Parser (const Nothing)
        p1 <|> p2 = Parser (\str -> case runParser p1 str of
                               Nothing -> case runParser p2 str of
                                              Nothing -> Nothing
                                              x -> x
                               x -> x)

intOrUppercase :: Parser ()
intOrUppercase = const () <$> posInt <|> const () <$> satisfy isUpper

