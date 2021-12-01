import Infinitable

countGreaters :: (Int, Infinitable Int, [Int]) -> (Int, Infinitable Int, [Int])
countGreaters (count, _, [])     = (count, Infinity, [])
countGreaters (count, _, [_])    = (count, Infinity, [])
countGreaters (count, _, [_, _]) = (count, Infinity, [])
countGreaters (count, (Finite prev), (x:y:z:zs)) | x+y+z > prev = countGreaters (count + 1, Finite (x+y+z), (y:z:zs))
countGreaters (count, prev,          (x:y:z:zs)) | otherwise    = countGreaters (count, Finite (x+y+z), (y:z:zs))

getAnswer :: String -> Int
getAnswer inp | otherwise     = fst3 $ countGreaters (0, Infinity, xs)
              where 
                xs = (\l -> read l :: Int) <$> lines inp
                fst3 = \((a, b, c)) -> a

day1 :: IO()
day1 = do
    inp <- readFile "./inputs/day01.txt"
    putStrLn $ show $ getAnswer inp