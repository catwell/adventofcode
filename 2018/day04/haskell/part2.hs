import Common

import Debug.Trace (traceShowId)

overallMostSleptMinute :: [SleepBlock] -> (Minute, Identifier)
overallMostSleptMinute blocks = (m, i) where
    f g = (mostSleptMinute' blocks g, g)
    l = map f $ allGuards blocks
    mx = maximum l
    m = snd $ fst mx
    i = snd mx

algo :: [SleepBlock] -> Int
algo blocks = fst m * snd m where
    m = overallMostSleptMinute blocks

main :: IO ()
main = do
    run algo
