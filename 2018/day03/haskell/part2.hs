import Common
import qualified Data.Map.Strict as Map

isMatch :: Matrix -> Matrix -> Bool
isMatch m x = (Map.intersection m x == x)

findMatch :: Matrix -> [Line] -> Int
findMatch _ [] = 0
findMatch m (x:xs) = if isMatch m (lineAsMap x)
    then identifier x
    else findMatch m xs

algo :: [Line] -> Int
algo l = findMatch (sumOfMaps l) l

main :: IO ()
main = do
    run algo
