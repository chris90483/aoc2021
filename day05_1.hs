import Data.List.Split

type Coord = (Int, Int)
type VentLine = (Coord, Coord)
type Diagram = [[Int]]
type DiagramDimensions = (Int, Int)

toTup :: Show a => [a] -> (a, a)
toTup []      = error "not enough elements in toTup: []"
toTup [x]     = error $ "not enough elements in toTup: [" ++ (show x) ++ "]"
toTup (x:y:_) = (x, y)

toInt :: Bool -> Int
toInt True  = 1
toInt False = 0

flatten :: [[a]] -> [a]
flatten xss = foldl1 (++) xss

parseCoord :: String -> Coord
parseCoord s = toTup $ map (\l -> read l :: Int) $ splitOn "," s

parseVentLine :: String -> VentLine
parseVentLine line = toTup $ map parseCoord $ map (filter (/= ' ')) $ splitOn "->" line

parse :: String -> [VentLine]
parse inp = map parseVentLine $ lines inp

inRange :: Int -> Int -> Int -> Bool
inRange n x y | x >  y    = n >= y && n <= x
inRange n x y | x == y    = n == x
inRange n x y | otherwise = n >= x && n <= y 

toDiagram :: VentLine -> Diagram
toDiagram ((x1, y1), (x2, y2)) | y1 == y2  = [[toInt (y == y1 && inRange x x1 x2) | x <- [0..(max x1 x2)]] | y <- [0..(max y1 y2)]]
toDiagram ((x1, y1), (x2, y2)) | x1 == x2  = [[toInt (x == x1 && inRange y y1 y2) | x <- [0..(max x1 x2)]] | y <- [0..(max y1 y2)]]
toDiagram ((x1, y1), (x2, y2)) | otherwise = emptyDiagram (max x1 x2, max y1 y2)

addRows :: [Int] -> [Int] -> [Int]
addRows []     ys     = ys
addRows xs     []     = xs
addRows (x:xs) (y:ys) = (x + y):(addRows xs ys)

addDiagram :: Diagram -> Diagram -> Diagram
addDiagram []       d2       = d2
addDiagram d1       []       = d1
addDiagram (xs:xss) (ys:yss) = (addRows xs ys):(addDiagram xss yss) 

addVentLine :: VentLine -> Diagram -> Diagram
addVentLine v d = d `addDiagram` (toDiagram v)

emptyDiagram :: DiagramDimensions -> Diagram
emptyDiagram (sizeX, sizeY) = take sizeY $ repeat $ take sizeX $ repeat 0

makeDiagram :: [VentLine] -> Diagram
makeDiagram []     = []
makeDiagram (v:vs) = addVentLine v $ makeDiagram vs

filterStraights :: [VentLine] -> [VentLine]
filterStraights []                                               = []
filterStraights (((x1, y1), (x2, y2)):vs) | y1 == y2 || x1 == x2 = ((x1, y1), (x2, y2)):(filterStraights vs) 
filterStraights (((x1, y1), (x2, y2)):vs) | otherwise            = filterStraights vs

getAnswer :: String -> Int
getAnswer inp = length $ filter (>1) $ flatten $ makeDiagram $ filterStraights $ parse inp

showDiagriam :: String -> String
showDiagriam inp = foldl1 (++) $ map (\line -> (show line) ++ "\n") (makeDiagram $ parse inp)

day5 :: IO()
day5 = do
    inp <- readFile "./inputs/day05.txt"
    putStrLn $ show $ getAnswer inp