-- | General shorthands and other small operations. Part of the "Useful" module.
module Useful.General where

import Data.List

-- | Alias of as /= (not equal to)
(!=) :: Eq a => a -> a -> Bool 
(!=) = (/=)


-- | Alias of mod
(%) :: (Integral a) => a -> a -> a
(%) = mod


-- | Works like python's \"in\" function. (Alias of elem). Simply checks if an item is in a list.
--
-- > $ "Hello" ? ["Hello","there","people"]
-- > True
-- > $ "Bonjour" ? ["Hello","there","people"]
-- > False
(?) :: Eq a => a -> [a] -> Bool
(?) = elem


-- | Alias of as isInFixOf. Checks if list is a sublist of another list
--
-- > $ "hello" ?? "Why hello there people"
-- > True
-- > $ [2,3] ?? [1,2,3,4]
-- > True
-- > $ "bonjour" ?? "why hello there people"
-- > False 
(??) :: Eq a => [a] -> [a] -> Bool
(??) = isInfixOf


-- | Returns the index of the first occurance of an item if it is in a list. Otherwise gives an error. Starts counting from 0!
--
-- NOTE: This is not like elemIndex! It does not return a Maybe Int it returns an error if the item is not in a list. Either use elemIndex or test using ? first.
-- 
-- > $ 'n' ?! "banana"
-- > 2
-- > $ 'v' ?! "banana"
-- > *** Exception: Item not in list
(?!) :: Eq a => a -> [a] -> Int
(?!) x y = f x y 0
	where
	f :: Eq a => a -> [a] -> Int -> Int
	f x [] _ = error "Item not in list"
	f x (y:ys) c
		|x == y = c
		|otherwise = f x ys (c+1)

-- | Takes a list and a pair (x,y) and inserts the item y into the list at position x
--
-- > $ ["hello","there","people"]  !/ (0,"bonjour")
-- > ["bonjour","there","people"]
(!/) :: [a] -> (Int,a) ->  [a]
(!/) xs (i,y)
	|i >= len xs = error "index too large"
	|otherwise = [if n == i then y else x | (x,n) <- zip xs [0..] ]

	
-- | Like !! but returns Maybe a
-- 
-- > $ [1,2,3,4] ! 5
-- > Nothing
-- > $ [1,2,3,4] ! 1
-- > Just 2
(!) :: [a] -> Int -> Maybe a
(!) xs i = plingHelper xs i 0

plingHelper [] _ _ = Nothing
plingHelper (x:xs) i n
	|i == n = Just x
	|otherwise = plingHelper xs i (n+1)
		
		
-- | alias of length
len :: [a] -> Int
len = length

-- | alias of length
count :: [a] -> Int
count = length


-- | Takes the unit list and returns the unit
--
-- NOTE: Will return an error if not supplied with the unit list
-- 
-- > $ the ["hello"]
-- > "hello"
-- > $ the ["hello","there"]
-- > "*** Exception: function 'the' called with a list other than the unit list.
the :: [a] -> a
the [x] = x
the _ = error "function 'the' called with a list other than the unit list."

