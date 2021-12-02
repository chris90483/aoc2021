import Data.List.Split

getAnswer :: String -> Int
getAnswer inp = let count word = sum $ map (\l -> read (last l) :: Int) $ filter ((==word) . head) (map (splitOn " ") (lines inp)) in count "forward" * (count "down" - count "up")

day2 :: IO()
day2 = do
    inp <- readFile "./inputs/day02.txt"
    putStrLn $ show $ getAnswer inp