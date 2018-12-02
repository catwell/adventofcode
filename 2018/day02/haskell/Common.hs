module Common where

import System.Environment

readStringInput :: IO String
readStringInput = do
    args <- getArgs
    let fn = head args
    r <- readFile fn
    return r
