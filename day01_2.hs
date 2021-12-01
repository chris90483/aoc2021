data Infinitable a = Infinity | Finite a deriving Show

instance Eq a => Eq (Infinitable a) where
    (==) Infinity Infinity     = True
    (==) Infinity (Finite x)   = False
    (==) (Finite x) Infinity   = False
    (==) (Finite x) (Finite y) = x == y
    (/=) Infinity Infinity     = False
    (/=) Infinity (Finite x)   = True
    (/=) (Finite x) Infinity   = True
    (/=) (Finite x) (Finite y) = x /= y

instance Ord a => Ord (Infinitable a) where
    compare Infinity Infinity     = EQ
    compare Infinity (Finite x)   = GT
    compare (Finite x) Infinity   = LT
    compare (Finite x) (Finite y) | x > y  = GT
                                  | x == y = EQ
                                  | x < y  = LT

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