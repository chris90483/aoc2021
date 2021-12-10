import Data.List.Split

data Stack a = Stack {elements :: [a]}

push :: Stack a -> a -> Stack a
push (Stack xs) x = Stack (x:xs)

pop :: Stack a -> (Stack a, a)
pop (Stack [])     = error "cannot pop from empty stack"
pop (Stack (x:xs)) = (Stack xs, x) 

isOpeningChar :: Char -> Bool
isOpeningChar c = elem c "([{<"

closes :: Char -> Char -> Bool
a `closes` b = b == '(' && a == ')' ||
               b == '[' && a == ']' || 
               b == '{' && a == '}' ||
               b == '<' && a == '>'

getIllegal :: String -> Stack Char -> Char
getIllegal []     (Stack [])                                = error "string is not illegal"
getIllegal []     stack                                     = error "unexpected EOL"
getIllegal (x:xs) (Stack []) | (not.isOpeningChar) x        = x
getIllegal (x:xs) stack      | isOpeningChar x              = getIllegal xs (stack `push` x)
getIllegal (x:xs) stack      | x `closes` (snd $ pop stack) = getIllegal xs (fst $ pop stack)
getIllegal (x:xs) stack      | otherwise                    = x

scoreOf :: Char -> Int
scoreOf ')' = 3
scoreOf ']' = 57
scoreOf '}' = 1197
scoreOf '>' = 25137
scoreOf _   = error "unknown character in scoreOf"

incomplete :: String -> Stack Char -> Bool
incomplete [] (Stack []) = False
incomplete [] stack      = True
incomplete (x:xs) stack | isOpeningChar x              = incomplete xs (stack `push` x)
incomplete (x:xs) stack | x `closes` (snd $ pop stack) = incomplete xs (fst $ pop stack)
incomplete (x:xs) stack | otherwise                    = False -- not incomplete, but corrupted

getAnswer :: String -> Int
getAnswer inp = sum [scoreOf illegal | illegal <- [getIllegal line (Stack []) | line <- filter (\l -> not $ incomplete l (Stack [])) $ lines inp]]

day10 :: IO()
day10 = do
    inp <- readFile "./inputs/day10.txt"
    putStrLn $ show $ getAnswer inp