module Configurator where

import System.Environment
import System.IO
import System.Directory (doesFileExist)
import Data.IORef
import Data.List.Split (splitOn)


type ConfName = String
type ConfValue = String
type Confs = IORef [(ConfName, IORef ConfValue)]

main :: String -> IO ()
main fileName = do
    --[fileName] <- getArgs

    putStrLn "Interactive options:"
    putStrLn "  * B <value> : modify previous value"
    putStrLn "  * W         : finish session and write to file"
    putStrLn "  * A         : abort session (discard all changes)"
    hFlush stdout

    createIfNotExists fileName
    file <- readFile fileName
    confs <- parseConfs file

    interactiveAction confs fileName

interactiveAction :: Confs -> FilePath -> IO ()
interactiveAction conf fname = do
    putStrLn "> Input property and value:"
    hFlush stdout
    input <- getLine
    let (command:args) = words input
    let arg = head args
    case command of
        "B" -> do
            value <- getValue conf arg
            let prevValue = maybe "undefined" id value
            putStrLn $ "Input new value for " ++ arg ++ " property (previous: " ++ prevValue ++ ")"
            hFlush stdout
            newValue <- getLine
            setValue conf arg newValue
            interactiveAction conf fname
        "W" -> do

            configs <- readIORef conf
            let getProp = \(name, valRef) -> do val <- readIORef valRef
                                                return (name ++ "=" ++ val ++ "\n")
            confs <- traverse getProp configs

            writeFile fname $ concat confs
            putStrLn $ "changes written in " ++ fname
            hFlush stdout
            interactiveAction conf fname
        "A" -> do
            putStrLn "Session aborted"
            hFlush stdout
            return ()
        _   -> do
            let (name:values) = splitOn "=" command
            setValue conf name $ head values
            interactiveAction conf fname


setValue :: Confs -> ConfName -> ConfValue -> IO ()
setValue conf name value = do
    configs <- readIORef conf
    case lookup name configs of
        Just valRef -> writeIORef valRef value
        Nothing     -> do valRef <- newIORef value
                          writeIORef conf ((name, valRef):configs)

getValue :: Confs -> ConfName -> IO (Maybe ConfValue)
getValue confs name = do
    conf <- readIORef confs
    case lookup name conf of
        Nothing -> return Nothing
        Just valueRef -> do value <- readIORef valueRef
                            return (Just value)

createIfNotExists :: FilePath -> IO ()
createIfNotExists fileName = do
    fileExist <- doesFileExist fileName
    if not fileExist
       then writeFile fileName ""
       else return ()

parseConfs :: String -> IO Confs
parseConfs "" = newIORef []
parseConfs strings = do
    let confs = splitOn "\n" strings
    let parseConf strConf = do
        let (name:vals) = take 2 $ splitOn "=" strConf
        let val = head vals
        valRef <- newIORef val
        return (name, valRef)
    result <- traverse parseConf confs
    newIORef $ result