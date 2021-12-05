import Data.List.Split
import Data.List

data Card = Card {
    rows :: [[Int]]
} deriving Show

data Bingo = Bingo {
    cards :: [Card],
    numbersLeft :: [Int],
    lastDrawn :: Int
} deriving Show

toColumns :: [[a]] -> [[a]]
toColumns = transpose

parseCards :: [String] -> [Card] -> [Card]
parseCards []      cs     = cs
parseCards ("":ss) []     = parseCards ss []
parseCards ("":ss) (c:cs) = c:(parseCards ss cs)
parseCards (s:ss)  []     = parseCards ss [Card [parseSeparated " " s]]
parseCards (s:ss)  (c:cs) = parseCards ss ((Card ((parseSeparated " " s):(rows c))):cs)

parseSeparated :: String -> String -> [Int]
parseSeparated s l = map (\l -> read l :: Int) $ filter (/="") $ splitOn s l

parse :: String -> Bingo
parse inp = Bingo cards numbersLeft (-1)
    where
        cards = parseCards cardsPart []
        numbersLeft = parseSeparated "," numberPart
        (numberPart:cardsPart) = lines inp

updateCards :: [Card] -> Int -> [Card]
updateCards cards numberDrawn = map (updateCard numberDrawn) cards
    where
        updatedRows card            = map (updateRow numberDrawn) (rows card)
        updateCard numberDrawn card = Card (updatedRows card)
        updateRow numberDrawn row   = map (markPlayed numberDrawn) row
        markPlayed numberDrawn num  = if (num == numberDrawn) then -1 else num
        
wonWith :: [[Int]] -> Bool
wonWith cardNums  = (length $ filter (\line -> wonOn line) cardNums) > 0
    where
        wonOn line        = and $ map (==(-1)) line        
        
hasWon :: Card -> Bool
hasWon card = wonWith (rows card) || wonWith (toColumns $ rows card)
        
makeWinner :: Bingo -> (Card, Int)
makeWinner bingo | hasWon (head $ cards bingo) = (head $ cards bingo, lastDrawn bingo)
                 | otherwise = makeWinner $ Bingo (updateCards (cards bingo) numberDrawn) nextNums numberDrawn
                             where
                                (numberDrawn:nextNums) = numbersLeft bingo

playBingo :: Bingo -> (Card, Int)
playBingo bingo | (length $ cards bingo) == 1    = makeWinner bingo
playBingo bingo | length (numbersLeft bingo) < 1 = (Card [[0]], 0)
playBingo bingo | otherwise                      = playBingo (step bingo)
    where
        step bingo             = Bingo (filter (not . hasWon) $ updateCards (cards bingo) numberDrawn) nextNums numberDrawn
        (numberDrawn:nextNums) = numbersLeft bingo

score :: Card -> Int -> Int
score card lastDrawn = lastDrawn * (sum $ map sum $ map (\line -> filter (/=(-1)) line) (rows card))


getAnswer :: String -> Int
getAnswer inp = uncurry score $ playBingo $ parse inp

day3 :: IO()
day3 = do
    inp <- readFile "./inputs/day04.txt"
    putStrLn $ show $ getAnswer inp
