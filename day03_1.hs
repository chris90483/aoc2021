getAnswer :: String -> Int
getAnswer inp = dec gamma * dec epsilon
              where
                bits = map (\l -> map (\c -> read [c] :: Int) l) $ lines inp
                gamma = map (flip quot $ (length bits) `quot` 2) $ foldl1 (zipWith (+)) bits
                epsilon = map (1-) gamma
                dec xs = sum $ zipWith (*) xs $ reverse [2^x | x <- take (length gamma) [0..]]
day3 :: IO()
day3 = do
    inp <- readFile "./inputs/day03.txt"
    putStrLn $ show $ getAnswer inp