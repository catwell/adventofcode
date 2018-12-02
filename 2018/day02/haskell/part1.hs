import Common (readStringInput)

occurrences :: String -> Char -> Int
occurrences s x = length $ filter (==x) s

hasn :: Int -> String -> Bool
hasn n s = foldr (||) False (map f s) where
    f c = (occurrences s c == n)

algo :: [String] -> Int
algo l = (c 2) * (c 3) where
    c n = sum (map (fromEnum . (hasn n)) l)

main :: IO ()
main = do
    stringInput <- readStringInput
    print $ algo (lines stringInput)
