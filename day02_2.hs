import Data.List.Split
import Useful.Dictionary

getAnswer :: String -> Int
getAnswer inp = (\(a,b,c) -> b * c) $ foldl (\s (cmd,val) -> ((dict [("forward", \(a,h,d) v -> (a,h+v,d+a*v)), ("up", \(a,h,d) v -> (a-v,h,d)), ("down", \(a,h,d) v -> (a+v,h,d))]) #!! cmd) s val) (0, 0, 0) (map (\l -> (head l, read (last l) :: Int)) $ map (splitOn " ") $ lines inp)

day2 :: IO()
day2 = do
    inp <- readFile "./inputs/day02.txt"
    putStrLn $ show $ getAnswer inp