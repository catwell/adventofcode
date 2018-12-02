import Common (readStringInput)
import qualified Data.Set as Set
import Data.Foldable (asum)

strMinus :: Int -> String -> String
strMinus n s = (take (n-1) s) ++ (drop n s)

findDup :: [String] -> Maybe String
findDup l = f l Set.empty where
    f [] _ = Nothing
    f (x:xs) set = if Set.member x set
        then Just x
        else f xs (Set.union (Set.singleton x) set)

findForLength :: Int -> [String] -> Maybe String
findForLength n input = findDup $ map (strMinus n) input

algo :: [String] -> Maybe String
algo input = asum [ findForLength n input | n <- [ 1 .. length input ] ]

main :: IO ()
main = do
    stringInput <- readStringInput
    case algo (lines stringInput) of
        Just answer -> putStrLn answer
        Nothing -> putStrLn "not found"
