-- | A lightweight Dictionary implementation based on Data.Map, part of the "Useful" module.
--
-- I like dictionaries, and use them often for small things when I'm coding, but the functions and 
-- syntax by default are hardly as elegant as something like python. This is one thing I feel is missing 
-- in the default implementation of data types. Also, the clashing namespace when importing Data.Map makes 
-- code often hard to read and long. Completely inconvenient for small, simple tasks.
-- This isn't a complete solution and nor is it optimal but it's lightweight and pretty.
-- Keys must have some ordering defined over them.
-- In the function descriptions, listed in square brackets are the Data.Map functions used - this does 
-- not mean it is an exact alias though, it make just use it.
module Useful.Dictionary where

import qualified Data.Map


-- * Dictionary creation

-- | Alias of Data.Map.fromList, takes a list of key value tuples and creates a dictionary out of them.
--
-- > dict [("hello",1),("there",2)]
-- > fromList [("hello",1),("there",2)]
-- > dict []
-- > fromList []
dict :: (Ord k) => [(k, a)] -> Data.Map.Map k a
dict l = Data.Map.fromList l

-- | Returns a List of key-value pairs
dictToList :: Data.Map.Map k a -> [(k, a)]
dictToList d = Data.Map.toList d

-- | Returns the size of a dictionary [Data.Map.size]
dictSize :: Data.Map.Map k a -> Int
dictSize d = Data.Map.size d

-- * Dictionary operations

-- | Returns Maybe v from key k [Data.Map.lookup]
(#!) :: (Ord k) => Data.Map.Map k a -> k -> Maybe a
d #! k = flip Data.Map.lookup d k

-- | Returns v from key k or error [Data.Map.!]
(#!!) :: (Ord k) => Data.Map.Map k a -> k -> a
(#!!) d k = (Data.Map.!) d k

-- | Adds key-value pair to a dictionary. If key is already in dict will update value. [Data.Map.insert]
(#+) :: (Ord k) => Data.Map.Map k a -> (k,a) -> Data.Map.Map k a
(#+) d (k,v) = Data.Map.insert k v d

-- | Checks for a key in a dictionary. [Data.Map.member]
(#?) :: (Ord k) => Data.Map.Map k a -> k -> Bool
(#?) d k = Data.Map.member k d

-- | Checks if a value is in a dictionary
(#*?) :: (Eq a, Ord k) => Data.Map.Map k a -> a -> Bool
(#*?) d v = (Data.Map.keys (Data.Map.filter (==v) d)) /= []

-- | Deletes a key-pair from a dictionary given a key [Data.Map.delete]
(#-) :: (Ord k) => Data.Map.Map k a -> k -> Data.Map.Map k a
(#-) d k = Data.Map.delete k d

-- | Deletes ALL key-pairs from a dictionary given they match a value [Data.Map.filter]
(#*-) :: (Eq a, Ord k) => Data.Map.Map k a -> a -> Data.Map.Map k a
(#*-) d v = Data.Map.filter (/=v) d

-- | Intersects two dictionaries [Data.Map.\\]
(#\\) :: (Ord k) => Data.Map.Map k a -> Data.Map.Map k b -> Data.Map.Map k a
(#\\) d1 d2 = (Data.Map.\\) d1 d2

-- | Unions two dictionaries [Data.Map.union]
(#++) :: (Ord k) => Data.Map.Map k a -> Data.Map.Map k a -> Data.Map.Map k a
(#++) d1 d2 = Data.Map.union d1 d2

-- | Tests if d1 is a sub-dictionary of d2 [Data.Map.isSubmapOf]
(#??) :: (Ord k, Eq a) => Data.Map.Map k a -> Data.Map.Map k a -> Bool
(#??) d1 d2 = Data.Map.isSubmapOf d1 d2

-- | Returns a the first occurance of a key from a value. Otherwise error.
(#?!) :: (Eq a, Ord k) => Data.Map.Map k a -> a -> k
(#?!) d v
	|(Data.Map.filter (==v) d) == Data.Map.empty = error "value is not in dictionary"
	|otherwise = head (Data.Map.keys (Data.Map.filter (==v) d))

-- * Dictionary mapping and filtering

-- | Maps a function to all values in a dictionary [Data.Map.map]
mapD :: (a -> b) -> Data.Map.Map k a -> Data.Map.Map k b
mapD func d = Data.Map.map func d

-- | Maps a function to all keys in a dictionary [Data.Map.mapKeys]
mapDkeys :: (Ord k2) => (k1 -> k2) -> Data.Map.Map k1 a -> Data.Map.Map k2 a
mapDkeys func d = Data.Map.mapKeys func d

-- | Filter over dictionary values [Data.Map.filter]
filterD :: (Ord k) => (a -> Bool) -> Data.Map.Map k a -> Data.Map.Map k a
filterD func d = Data.Map.filter func d

-- | Filter over keys in a dictionary [Data.Map.filterWithKey]
filterDkeys :: (Ord k) => (k -> a -> Bool) -> Data.Map.Map k a -> Data.Map.Map k a
filterDkeys func d = Data.Map.filterWithKey func d
