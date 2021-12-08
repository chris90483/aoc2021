import Data.List.Split
import Useful.Dictionary
import Data.List
import Data.Maybe

import qualified Data.Map

allAppearIn :: (Eq a) => [a] -> [a] -> Bool
allAppearIn []     ys = True
allAppearIn (x:xs) ys = elem x ys && allAppearIn xs ys 

countAppearences :: (Eq a) => [a] -> [a] -> Int
countAppearences []     ys = 0
countAppearences (x:xs) ys = if elem x ys then 1+(countAppearences xs ys) else countAppearences xs ys

solveInput5 :: String -> Data.Map.Map String Int -> Data.Map.Map String Int
solveInput5 input mappings | (mappings #?! 7) `allAppearIn` input         = mappings #+ (input, 3)
                           | countAppearences (mappings #?! 4) input == 3 = mappings #+ (input, 5)
                           | otherwise                                    = mappings #+ (input, 2)

solveInput6 :: String -> Data.Map.Map String Int -> Data.Map.Map String Int
solveInput6 input mappings |  not $ (mappings #?! 7) `allAppearIn` input                                          = mappings #+ (input, 6)
                           | (not $ (mappings #?! 4) `allAppearIn` input) && (mappings #?! 7) `allAppearIn` input = mappings #+ (input, 0)
                           | otherwise                                                                            = mappings #+ (input, 9)

getOtherMappings :: [String] -> Data.Map.Map String Int -> Data.Map.Map String Int
getOtherMappings []     mappings                 = mappings
getOtherMappings (x:xs) mappings | length x == 5 = getOtherMappings xs (solveInput5 x mappings)
                          | length x == 6 = getOtherMappings xs (solveInput6 x mappings)
                          | otherwise     = getOtherMappings xs mappings

knownMappings :: [String] -> Data.Map.Map String Int -> Data.Map.Map String Int
knownMappings []     d                 = d
knownMappings (x:xs) d | length x == 2 = knownMappings xs (d #+ (x, 1))
                       | length x == 3 = knownMappings xs (d #+ (x, 7))
                       | length x == 4 = knownMappings xs (d #+ (x, 4))
                       | length x == 7 = knownMappings xs (d #+ (x, 8))
                       | otherwise     = knownMappings xs d

toValidKey :: Data.Map.Map String Int -> String -> String
toValidKey d s = fst $ head $ filter (\(k, v) -> (sort k) == (sort s)) $ dictToList d

solveLine :: [[String]] -> [Int]
solveLine [inputs, outputs] = map (\outVal -> mappings #!! (toValidKey mappings outVal)) outputs
    where
        mappings = getOtherMappings inputs (knownMappings inputs $ dict [])

solve :: [[[String]]] -> [[Int]]
solve lines = map solveLine lines

parse :: String -> [[[String]]]
parse inp = map (map (splitOn " ")) $ map (splitOn " | ") $ lines inp

digitAppend :: Int -> String -> String
digitAppend n s = (head $ show n):s

getAnswer :: String -> Int
getAnswer inp = sum $ map (\l -> read l :: Int) $ map (foldr digitAppend "") $ solve $ parse inp

day8 :: IO()
day8 = do
    inp <- readFile "./inputs/day08.txt"
    putStrLn $ show $ getAnswer inp