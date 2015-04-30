module Main where

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
            getContents
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
