import Common (run)
import qualified Data.Set as Set

algo :: [Int] -> Int
algo l = f (cycle l) 0 (Set.singleton 0) where
    f (x:xs) s found =
        let
            s' = s + x
        in
            if Set.member s' found
                then s'
                else f xs s' (Set.union found (Set.singleton s'))

main :: IO ()
main = do
    run algo
