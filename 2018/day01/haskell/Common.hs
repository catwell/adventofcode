module Common where

import System.Environment
import Text.Parsec
import Text.Parsec.String (Parser)

readStringInput :: IO String
readStringInput = do
    args <- getArgs
    let fn = head args
    r <- readFile fn
    return r

numberCharSet = "-0123456789"

isANumber :: Parser String
isANumber = many1 $ oneOf numberCharSet

isNotANumber :: Parser String
isNotANumber = many1 $ noneOf numberCharSet

numberParser :: Parser Int
numberParser = read <$> isANumber

inputParser :: Parser [Int]
inputParser = do
    skipMany $ noneOf numberCharSet
    numbers <- endBy numberParser isNotANumber
    return numbers

run :: ([Int] -> Int) -> IO ()
run algo = do
    stringInput <- readStringInput
    case parse inputParser "" stringInput of
        Right input -> print $ algo input
        Left err -> print err
