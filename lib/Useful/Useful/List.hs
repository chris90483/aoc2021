-- | List operations, part of the "Useful" module.
module Useful.List where

import Useful.General
import Data.List

-- | Takes a list item and splits a list around it, removing the item.
--
-- > $ explodeI "Hello there people" ' '
-- > ["Hello","there","people"]
explodeI :: Eq a => [a] -> a -> [[a]]
explodeI xs m = rmEmpty (explode' xs m)

-- explode' :: Eq a => [a] -> a -> [[a]]
explode' [] m = []
explode' xs m = [takeWhile (!=m) xs] ++ explode' (tail' (dropWhile (!=m) xs)) m
	where
	tail' [] = []
	tail' (x:xs) = xs

-- | Alias of explodeI
splitI :: Eq a => [a] -> a -> [[a]]
splitI = explodeI
	
	
-- | Take a list item and concatinates each element of it around another given item.
--
-- > $implodeI "askjdnaskd" '!'
-- > "a!s!k!j!d!n!a!s!k!d"
implodeI :: Eq a => [a] -> a -> [a]
implodeI (x:xs) y
	|xs == [] = [x]
	|otherwise = (x : [y]) ++ (implodeI xs y)

-- | alias of implodeI
joinI :: Eq a => [a] -> a -> [a]
joinI = implodeI


-- | Takes a two lists and explodes the first into a new list, around the second. Removing the second list where it occurs.
--
-- > $explode "hello there people" "ll"
-- > ["he","o there people"]
-- > $explode "hello there people" " "
-- > ["hello","there","people"]
explode :: Eq a => [a] -> [a] -> [[a]]
explode x y = explode'' x y 0 0

-- explode'' :: Eq a => [a] -> [a] -> Int -> Int -> [[a]]
explode'' x y buff count
	|x == y  = []
	|x == [] = []
	|y == [] = [x]
	|count+buff == len x = [x]
	|(len y) == buff = (fst splut) : (explode'' (drop buff (snd splut)) y 0 0) -- If the buffer is full (there is a full match) then split the string and explode the rest.
	|(x !! (count+buff)) == (y !! buff) = explode'' x y (buff+1) count -- If the character matches increase the buffer
	|otherwise = explode'' x y 0 (count+1) -- otherwise just increment the counter.
		where splut = (splitAt (count) x)

-- | alias of explode
split :: Eq a => [a] -> [a] -> [[a]]
split = explode


-- | Takes a list of lists and an extra list and concatinates the list of lists with the second list inbetween. When used with the empty list mimics concat
--
-- > $ implode ["helloasdad","asd hello","hello"] "!!"
-- > "helloasdad!!asd hello!!hello"
implode :: Eq a => [[a]] -> [a] -> [a]
implode x [] = concat x
implode (x:xs) y
	|xs == [] = x
	|otherwise = (x ++ y) ++ (implode xs y)

-- | Alias of implode
join :: Eq a => [[a]] -> [a] -> [a]
join = implode

	
-- | takes a number of items from a list before it reaches the index n
-- 
-- > $ takeBefore 5 "Hello there people"
-- > "Hello there p"
-- takeBefore :: Eq a => Int -> [a] -> [a]
-- takeBefore n x = take (len x - n) x 0


-- | drops a number of items from a list before it reaches the index n
--
-- > $ dropBefore 5 "Hello there people"
-- > "eople"
-- dropBefore :: Eq a => Int -> [a] -> [a]
-- dropBefore n x = drop (len x - n) x 0


-- | In a list of lists this removes any occurances of the empty list. Can also be used to remove occurances of the empty string.
--
-- > $rmEmpty ["hello","","there","","people",""]
-- > ["hello","there","people"]
rmEmpty :: Eq a =>  [[a]] -> [[a]]
rmEmpty [] = []
rmEmpty (x:xs)
	|x == [] = rmEmpty xs
	|otherwise = x:(rmEmpty xs)


-- | maps a function in depth N to the given list. map3, map4, map5 are also defined.
--
-- > $ map2 (*2) [[1,2,3,4],[1,1,1,2]]
-- > [[2,4,6,8],[2,2,2,4]]
map2 :: (a -> b) -> [[a]] -> [[b]]
map2 f x = map (map f) x

map3 :: (a -> b) -> [[[a]]] -> [[[b]]]
map3 f x = map (map (map f)) x

map4 :: (a -> b) -> [[[[a]]]] -> [[[[b]]]]
map4 f x = map (map (map (map f))) x

map5 :: (a -> b) -> [[[[[a]]]]] -> [[[[[b]]]]]
map5 f x = map (map (map (map (map f)))) x

-- | Replaces any occurrences of the second list, with the third list, in the first list.
--
-- > $ replace "why hello hello there" "hello" "bonjour"
-- > "why bonjour bonjour there"
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace s [] x = s
replace [] _ _ = []
replace s find repl
    |take (length find) s == find = repl ++ (replace (drop (length find) s) find repl)
    |otherwise = [head s] ++ (replace (tail s) find repl)
	

-- | Takes a list of items and returns a list with each element in it's own single list.
--
-- > $ each "hello"
-- > ["h","e","l","l","o"]
each :: [a] -> [[a]]
each x = f x
	where
	f :: [a] -> [[a]]
	f [] = []
	f (x:xs) = [x] : f xs
