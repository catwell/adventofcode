import Common (run)

algo :: [Int] -> Int
algo = sum

main :: IO ()
main = do
    run algo
