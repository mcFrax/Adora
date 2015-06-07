module Main where

import System.Environment
import System.Exit(exitFailure)
import System.FilePath
import System.IO

import ErrM
import Paradora
import Layoutadora

main :: IO ()
main = do
    args <- getArgs
    case args of
        [sourcePath, hsPath, varName] -> do
            source <- readFile sourcePath
            tree <- case pModule $ resolveLayout True . myLexer $ source of
                Bad errmsg -> do
                    hPutStrLn stderr $ "parselib: Parser error:\n" ++ errmsg ++ "\n"
                    exitFailure
                Ok tree -> return tree
            hsFile <- openFile hsPath WriteMode
            hPutStrLn hsFile ("-- Generated by parselib from " ++ sourcePath ++
                              "\nmodule " ++ (takeBaseName hsPath) ++ " where" ++
                              "\n\n" ++ "import Absadora" ++
                              "\n\n" ++ varName ++ " :: Module" ++
                              "\n" ++ varName ++ " = " ++ (show tree))
            hClose hsFile
        _ -> do
            progName <- getProgName
            hPutStrLn stderr ("Usage:\n    " ++ progName ++
                              " <source> <hsPath> <varName>\n")
            exitFailure
