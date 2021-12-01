countGreaters :: (Int, Int) -> Int -> (Int, Int)
countGreaters (count, prev) y | y > prev  = (count + 1, y)
countGreaters (count, prev) y | otherwise = (count, y)

getAnswer :: String -> Int
getAnswer inp = fst $ foldl countGreaters (0, x) xs
              where
                (x:xs) = (\l -> read l :: Int) <$> lines inp

day1 :: IO()
day1 = do
    inp <- readFile "./inputs/day01.txt"
    putStrLn $ show $ getAnswer inp