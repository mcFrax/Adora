module Main where

import qualified Data.Map as M
import System.Environment
import System.Exit(exitFailure)
import System.IO

import Absadora
import ErrM
import Paradora

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
    case pModule $ myLexer $ code of
        Bad errmsg -> do
            hPutStrLn stderr $ "Parser error:\n" ++ errmsg ++ "\n"
            exitFailure
        Ok moduleSyntax -> do
            case moduleSem moduleSyntax of
                Left errmsg -> do
                    hPutStrLn stderr $ showSemError errmsg
                    exitFailure
                Right runModule -> runModule
