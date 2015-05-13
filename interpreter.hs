module Main where

#ifdef USE_HASKELINE
import System.Console.Haskeline
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
    code <- case args of
        [] -> do
            readStdin
        [programPath] -> do
            readFile programPath
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
                    hPutStrLn stderr $ showSemError errmsg
                    exitFailure
                Right runModule -> runModule
    where
#ifdef USE_HASKELINE
        readStdin = do
            runInputT defaultSettings readLines
            where
                readLines :: InputT IO String
                readLines = do
                    maybeLine <- getInputLine ">"
                    case maybeLine of
                        Nothing -> return ""
                        Just ln -> do
                            rest <- readLines
                            return $ ln ++ "\n" ++ rest
#else
        readStdin = getContents
#endif
