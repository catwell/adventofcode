import Common

algo :: [SleepBlock] -> Int
algo blocks = sleeperId * minute where
    sleeperId = mostSleepingGuard blocks
    minute = mostSleptMinute blocks sleeperId

main :: IO ()
main = do
    run algo
