import Data.List.Split
import BinTree
import Useful.Dictionary

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

getClosingChars :: String -> Stack Char -> String
getClosingChars []     (Stack [])                                = ""
getClosingChars []     stack                                     = map (\c -> dict [('(',')'),('[',']'),('{','}'),('<','>')] #!! c) $ elements stack
getClosingChars (x:xs) (Stack []) | (not.isOpeningChar) x        = error "the string is corrupted. We found a closing char but the stack is empty."
getClosingChars (x:xs) stack      | isOpeningChar x              = getClosingChars xs (stack `push` x)
getClosingChars (x:xs) stack      | x `closes` (snd $ pop stack) = getClosingChars xs (fst $ pop stack)
getClosingChars (x:xs) stack      | otherwise                    = error "the string is corrupted. The char doesn't close the char at the top of the stack."

scoreOf :: String -> Int
scoreOf s = scoreOf' s 0
    where
        scoreOf' [] prevScore = prevScore
        scoreOf' (x:xs) prevScore = scoreOf' xs ((prevScore * 5) + scoreOfChar x)
        scoreOfChar c = (dict [(')', 1),(']',2),('}',3),('>',4)]) #!! c

middle :: [Int] -> Int
middle xs = xs !! ((length xs) `div` 2)

incomplete :: String -> Stack Char -> Bool
incomplete [] (Stack []) = False
incomplete [] stack      = True
incomplete (x:xs) stack | isOpeningChar x              = incomplete xs (stack `push` x)
incomplete (x:xs) stack | x `closes` (snd $ pop stack) = incomplete xs (fst $ pop stack)
incomplete (x:xs) stack | otherwise                    = False -- not incomplete, but corrupted

getAnswer :: String -> Int
getAnswer inp = middle $ binTreeSort $ [scoreOf closingChars | closingChars <- [getClosingChars line (Stack []) | line <- filter (\l -> incomplete l (Stack [])) $ lines inp]]

day10 :: IO()
day10 = do
    inp <- readFile "./inputs/day10.txt"
    putStrLn $ show $ getAnswer inp