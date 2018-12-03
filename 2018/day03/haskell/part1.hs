import Common
import qualified Data.Map.Strict as Map

algo :: [Line] -> Int
algo l = Map.size $ Map.filter (> 1) $ sumOfMaps l

main :: IO ()
main = do
    run algo
