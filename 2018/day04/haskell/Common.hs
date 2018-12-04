module Common where

import System.Environment

import Data.Tuple (swap)
import Data.List (sort, isInfixOf)
import Data.Map (Map, fromListWith, toList)
import qualified Data.Set as Set

import Text.Parsec (many1, parse)
import Text.Parsec.Char (char, digit, noneOf)
import Text.Parsec.String (Parser)

type Minute = Int
type Identifier = Int

data Line =
    GuardChange Identifier
    | GuardSleep Minute
    | GuardAwake Minute
    | BadLine
    deriving (Show)

data SleepBlock = SleepBlock Identifier Minute Minute

readStringInput :: IO String
readStringInput = do
    args <- getArgs
    let fn = head args
    r <- readFile fn
    return r

numberParser :: Parser Int
numberParser = read <$> many1 digit

numberAfterParser :: Char -> Parser Int
numberAfterParser c = do
    many1 $ noneOf [c]
    char c
    r <- numberParser
    return r

minuteParser :: Parser Minute
minuteParser = numberAfterParser ':'

guardIdParser :: Parser Identifier
guardIdParser = numberAfterParser '#'

parseGuard :: String -> Line
parseGuard l = case parse guardIdParser "" l of
    Right i -> GuardChange i
    Left err -> BadLine

parseSleep :: String -> Line
parseSleep l = case parse minuteParser "" l of
    Right m -> GuardSleep m
    Left err -> BadLine

parseAwake :: String -> Line
parseAwake l = case parse minuteParser "" l of
    Right m -> GuardAwake m
    Left err -> BadLine

parseLine :: String -> Line
parseLine l
    | isInfixOf "Guard" l = parseGuard l
    | isInfixOf "wakes" l = parseAwake l
    | isInfixOf "asleep" l = parseSleep l
    | otherwise = BadLine

parseLines :: [String] -> [Line]
parseLines l = map parseLine l

sleepBlocks :: [Line] -> [SleepBlock]
sleepBlocks l = f 0 0 l [] where
    f _ _ [] r = r
    f i t (x:xs) r = case x of
        GuardChange i' -> f i' 0 xs r
        GuardSleep t' -> f i t' xs r
        GuardAwake t' -> f i 0 xs ((SleepBlock i t t') : r)
        otherwise -> f i t xs r

guardId :: SleepBlock -> Identifier
guardId (SleepBlock i _ _) = i

firstSleepMinute :: SleepBlock -> Minute
firstSleepMinute (SleepBlock _ m _) = m

lastSleepMinute :: SleepBlock -> Minute
lastSleepMinute (SleepBlock _ _ m) = m - 1

sleepTime :: SleepBlock -> Int
sleepTime (SleepBlock _ m1 m2) = m2 - m1

isGuard :: Identifier -> SleepBlock -> Bool
isGuard i b = case b of
    SleepBlock i' _ _ -> (i' == i)

minutesSleptForGuard :: [SleepBlock] -> Identifier -> Int
minutesSleptForGuard blocks i = sum $ map sleepTime $ filter (isGuard i) blocks

mostSleepingGuard :: [SleepBlock] -> Identifier
mostSleepingGuard blocks = snd $ maximum $ map f (allGuards blocks) where
    f g = (minutesSleptForGuard blocks g, g)

histogramForGuard :: [SleepBlock] -> Identifier -> (Map Minute Int)
histogramForGuard blocks i = fromListWith (+) l where
    validBlocks = filter (isGuard i) blocks
    cells b = [(m, 1 :: Int) | m <- [firstSleepMinute b .. lastSleepMinute b]]
    l = concat [ cells b | b <- validBlocks ]

mostSleptMinute' :: [SleepBlock] -> Identifier -> (Int, Minute)
mostSleptMinute' blocks i = maximum $ map swap (toList h) where
    h = histogramForGuard blocks i

mostSleptMinute :: [SleepBlock] -> Identifier -> Minute
mostSleptMinute blocks i = snd $ mostSleptMinute' blocks i

allGuards :: [SleepBlock] -> [Identifier]
allGuards blocks = Set.toList $ Set.fromList $ map guardId blocks

run :: ([SleepBlock] -> Int) -> IO ()
run algo = do
    stringInput <- readStringInput
    print $ algo $ sleepBlocks $ parseLines $ sort $ lines stringInput

