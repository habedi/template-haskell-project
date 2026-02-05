{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (when)
import Lib (add, greet)
import System.Environment (getArgs, getProgName)

main :: IO ()
main = do
    let result = add 10 5
    putStrLn $ "Result of add(10, 5) is: " ++ show result

    greet "World"

    progName <- getProgName
    args <- getArgs

    putStrLn "\nCommand line arguments:"
    putStrLn $ "  Program: " ++ progName
    mapM_ (\arg -> putStrLn $ "  Arg: " ++ arg) args

    when (null args) $
        putStrLn "\nTry running with arguments: stack run -- arg1 arg2"
