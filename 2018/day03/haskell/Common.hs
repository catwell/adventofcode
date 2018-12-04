{-# LANGUAGE RecordWildCards #-} -- for {..}

module Common where

import System.Environment
import Text.Parsec (many1, endBy, parse)
import Text.Parsec.Char
import Text.Parsec.String (Parser)
import qualified Data.Map.Strict as Map

data Line = Line {
    identifier :: Int,
    x :: Int,
    y :: Int,
    w :: Int,
    h :: Int
} deriving Show

readStringInput :: IO String
readStringInput = do
    args <- getArgs
    let fn = head args
    r <- readFile fn
    return r

numberParser :: Parser Int
numberParser = read <$> many1 digit

-- Example: #1 @ 1,3: 4x4
lineParser :: Parser Line
lineParser = do
    char '#'
    identifier <- numberParser
    char ' '
    char '@'
    char ' '
    x <- numberParser
    char ','
    y <- numberParser
    char ':'
    char ' '
    w <- numberParser
    char 'x'
    h <- numberParser
    return Line {..}

inputParser :: Parser [Line]
inputParser = do
    r <- endBy lineParser endOfLine
    return r

type Matrix = Map.Map (Int, Int) Int

lineAsMap :: Line -> Matrix
lineAsMap l = Map.fromList
    [ ((i, j), 1) | i <- [x l .. x l + w l - 1], j <- [y l .. y l + h l - 1] ]

sumOfMaps :: [Line] -> Matrix
sumOfMaps l = Map.unionsWith (+) $ map lineAsMap l

run :: ([Line] -> Int) -> IO ()
run algo = do
    stringInput <- readStringInput
    case parse inputParser "" stringInput of
        Right input -> print $ algo input
        Left err -> print err

