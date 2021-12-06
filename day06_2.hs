import Data.List.Split
import Useful.General
import Useful.Dictionary
import Data.Maybe

import qualified Data.Map

-- First some definitions
(⚗) :: (a -> Bool) -> [a] -> [a]
f ⚗ xs = filter f xs

(🗺) :: (a -> b) -> [a] -> [b]
f 🗺 xs = map f xs

-- Ok here we go

parse :: String -> [Int]
parse inp = (\l -> read l :: Int) 🗺 splitOn "," inp

(🧮) :: Data.Map.Map Int Int -> [Int] -> Data.Map.Map Int Int
d 🧮 []     = d
d 🧮 (x:xs) = (d #+ (x, (length $ (==x) ⚗ (x:xs)))) 🧮 ((/=x) ⚗ (xs))

(🤷) :: (Data.Map.Map Int Int) -> Int -> Int
d 🤷 k | (k #! d) == Nothing = 0
d 🤷 k | otherwise           = d #!! k

(👴) :: Int -> Data.Map.Map Int Int -> Data.Map.Map Int Int
v 👴 d | (0 #! d) == Nothing = v 👴 d #+ (0, 0)
v 👴 d | otherwise           = d #+ (8, v) #+ (6, ((d 🤷 6) + v))

(👵) :: Data.Map.Map Int Int -> Int -> Data.Map.Map Int Int
d 👵 k | k == 8    = d #+ (8, 0) -- all eights shall disappear, until 0 adds them again
d 👵 k | otherwise = d #+ (k, d 🤷 (k + 1))

(👞👞) ::  [(Int, Int)] -> Data.Map.Map Int Int -> Data.Map.Map Int Int
kvs 👞👞 d = (d 🤷 0) 👴 foldl (👵) d [0..8]

(🐠) :: Int -> Data.Map.Map Int Int -> Int
0    🐠 occs = foldl1 (+) $ snd 🗺 (dictToList occs)
days 🐠 occs = (🐠) (days - 1) ((dictToList occs) 👞👞 occs)

(🐟) :: Int -> [Int] -> Int
days 🐟 fishes = (🐠) days $ (dict []) 🧮 fishes

getAnswer :: String -> Int
getAnswer inp = (🐟) 256 $ parse inp

day6 :: IO()
day6 = do
    inp <- readFile "./inputs/day06.txt"
    putStrLn $ show $ getAnswer inp