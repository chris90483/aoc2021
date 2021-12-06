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

diff :: Int -> Int -> Int
diff x y | x < y     = y - x
diff x y | otherwise = x - y

isDiagonal :: Coord -> Coord ->  Bool
isDiagonal (x1, y1) (x2, y2) = diff x1 x2  == diff y1 y2

newVentLine :: VentLine -> VentLine
newVentLine ((x1, y1), (x2, y2)) | x1 == x2 && y1 <  y2 = ((x1,     y1 + 1), (x2, y2))
                                 | x1 == x2 && y1 >  y2 = ((x1,     y1 - 1), (x2, y2))
                                 | x1 <  x2 && y1 == y2 = ((x1 + 1, y1    ), (x2, y2))
                                 | x1 >  x2 && y1 == y2 = ((x1 - 1, y1    ), (x2, y2))
                                 | x1 <  x2 && y1 <  y2 = ((x1 + 1, y1 + 1), (x2, y2))
                                 | x1 <  x2 && y1 >  y2 = ((x1 + 1, y1 - 1), (x2, y2))
                                 | x1 >  x2 && y1 <  y2 = ((x1 - 1, y1 + 1), (x2, y2))
                                 | x1 >  x2 && y1 >  y2 = ((x1 - 1, y1 - 1), (x2, y2))

getCoords :: VentLine -> [Coord]
getCoords ((x1, y1), (x2, y2)) | x1 == x2 && y1 == y2 = [(x1, y1)]
getCoords (start, end) = start:(getCoords $ newVentLine (start, end))

addCol :: Int -> [Int] -> [Int]
addCol i ys = (take i ys) ++ (((ys !! i) + 1):(drop (i + 1) ys))

addPoint :: (Int, Int) -> Diagram -> Diagram
addPoint (x, y) d = (take x d) ++ ((addCol y (d !! x)):(drop (x + 1) d))

addVentLine :: VentLine -> Diagram -> Diagram
addVentLine v d = foldr addPoint d coords
    where coords = getCoords v

emptyDiagram :: DiagramDimensions -> Diagram
emptyDiagram (sizeX, sizeY) = take sizeX $ repeat $ take sizeY $ repeat 0

makeDiagram :: [VentLine] -> Diagram
makeDiagram []     = []
makeDiagram vs = foldr addVentLine (emptyDiagram (maxX vs, maxY vs)) vs
    where
        maxX ventlines = (maximum $ [max x1 x2 | ((x1, y1), (x2, y2)) <- ventlines]) + 1
        maxY ventlines = (maximum $ [max y1 y2 | ((x1, y1), (x2, y2)) <- ventlines]) + 1


getAnswer :: String -> Int
getAnswer inp = length $ filter (>1) $ flatten $ makeDiagram $ parse inp

showDiagram :: String -> String
showDiagram inp = foldl1 (++) $ map (\line -> (show line) ++ "\n") (makeDiagram $ parse inp)

day5 :: IO()
day5 = do
    inp <- readFile "./inputs/day05.txt"
    putStrLn $ show $ getAnswer inp