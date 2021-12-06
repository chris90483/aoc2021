import Data.List.Split

parse :: String -> [Int]
parse inp = map (\l -> read l :: Int) $ splitOn "," inp

step :: [Int] -> [Int]
step []     = []
step (0:ts) = 6:(step ts) ++ [8]
step (t:ts) = (t-1):(step ts)

simulate :: [Int] -> [[Int]]
simulate ts = ts:(simulate $ step ts)

pop :: Int -> [Int] -> Int
pop days init = length $ last $ take (days + 1) $ simulate init

getAnswer :: String -> Int
getAnswer inp = pop 80 (parse inp)

showProgression days init = foldl1 (++) $ map (\line -> (show line) ++ "\n") $ take (days + 1) $ simulate init

day6 :: IO()
day6 = do
    inp <- readFile "./inputs/day06.txt"
    putStrLn $ show $ getAnswer inp