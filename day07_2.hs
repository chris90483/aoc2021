import Data.List.Split

diff :: Int -> Int -> Int
diff a b = abs $ a - b

cost :: [Int] -> Int -> Int
cost xs n = sum $ map (\x -> ((diff x n)*((diff x n) + 1)) `div` 2) xs

parse :: String -> [Int]
parse inp = map (\l -> read l :: Int) $ splitOn "," inp

getAnswer :: String -> Int
getAnswer inp = minimum $ map (cost positions) [(minimum positions)..(maximum positions)]
    where
        positions = parse inp

day7 :: IO()
day7 = do
    inp <- readFile "./inputs/day07.txt"
    putStrLn $ show $ getAnswer inp