module Main where

#ifdef USE_HASKELINE
import Control.Monad
import System.Console.Haskeline
import System.FilePath
#endif
import System.Environment
import System.Exit(exitFailure)
import System.IO

import ErrM
import Paradora
import Layoutadora

import Semantic

main :: IO ()
main = do
    args <- getArgs
    (code, programPath) <- case args of
        [] -> do
            code <- readStdin
            return (code, "<stdin>")
        [programPath] -> do
            code <- readFile programPath
            return (code, programPath)
        _ -> do
            progName <- getProgName
            hPutStrLn stderr $ "Usage:\n    " ++ progName ++ " [FILE PATH]\n"
            exitFailure
    case pModule $ resolveLayout True . myLexer $ code of
        Bad errmsg -> do
            hPutStrLn stderr $ "Parser error:\n" ++ errmsg ++ "\n"
            exitFailure
        Ok moduleSyntax -> do
            case moduleSem moduleSyntax of
                Left errmsg -> do
                    hPutStrLn stderr $ showSemError programPath errmsg
                    exitFailure
                Right runModule -> runModule
    where
#ifdef USE_HASKELINE
        readStdin = do
            histfile <- do
                liftM (`replaceFileName` ".adora-history") getExecutablePath
            runInputT defaultSettings{historyFile=Just histfile} readLines
            where
                readLines :: InputT IO String
                readLines = do
                    maybeLine <- getInputLine "> "
                    case maybeLine of
                        Nothing -> return ""
                        Just ln -> do
                            rest <- readLines
                            return $ ln ++ "\n" ++ rest
#else
        readStdin = getContents
#endif
