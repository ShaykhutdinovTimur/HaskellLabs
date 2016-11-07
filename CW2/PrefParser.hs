--функция должна считать все слова из файла, начианающиеся с префикса, заданного из командной строки
--затем после каждой команды программа выводит следущее слово в порядке очередности
module PrefParser where

import Control.Monad.Trans.State
import Control.Monad
import System.Environment (getArgs)

type PrefParser a = StateT String Maybe a

mainFun :: IO ()
mainFun = do
    (arg1, arg2) <- getArgs
    mainFun' arg1 arg2

mainFun' :: FilePath -> String -> IO ()
mainFun' file prefix = do
    src <- readFile file
    interactiveParser src prefix

interactiveParser :: String -> String -> IO ()
interactiveParser src prefix = do
    command <- getLine
    case command of "stop" -> putStrLn "iterrupted"
                    _      -> case runStateT (parseFile prefix) src of Nothing             -> putStrLn "no more words"
                                                                       Just (parsed, src') -> putStrLn parsed >> interactiveParser src' prefix
parseFile :: String -> PrefParser String
parseFile prefix = StateT $ \str -> parsePref str prefix

parsePref :: String -> String -> Maybe (String, String)
parsePref [] pref = Nothing
parsePref str pref = if isPref str pref then Just (head words str, skipword str)
                                        else parsePref (skipword str) pref

skipword :: String -> String
skipword = unwords . tail . words

isPref :: String -> String -> Bool
isPref [] [] = True
isPref [] _ = False
isPref str pref = if head str == head str then isPref (tail str) (tail pref) else False