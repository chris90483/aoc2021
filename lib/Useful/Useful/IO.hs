-- | IO operations, part of the "Useful" module.
module Useful.IO where

import Useful.General
import System.Random


-- | repeats an IO function n number of times.
replicateM :: (Monad m) => Int -> m a -> m [a]
replicateM n x = sequence (replicate n x)

-- | Like replicateM but stores the returns
replicateM_ :: (Monad m) => Int -> m a -> m ()
replicateM_ n x = sequence_ (replicate n x)



-- | repeats an IO function for every member of a list, using the list item as an arguement
-- 
-- > $ do foreach [1..3] print
-- > 1
-- > 2
-- > 3
-- > $ do foreach "asjkdnsd" putChar
-- > asjkdnsd
foreach :: Monad m => [a] -> (a -> m b) -> m ()
foreach = flip mapM_


-- | repeats an IO function until a IO Bool is true
--
-- NOTE: Be careful with this function! Better to use recursion. Testing against an item created in the loop will not work.
while :: (Monad m) => m Bool -> m a -> m ()
while test action = do
  val <- test
  if val then do {action;while test action}
         else return ()


-- | like putStr or putChar but works on any type with \"show\" defined in a similar way to how print does. Can be thought of as \"print\" without the trailing linebreak.
--
-- NOTE: This means it will print strings with quotes around them. To print strings without quotes use putStr or putStrLn
put :: Show a => a -> IO ()
put x = putStr (show x)

-- | Alias of put
write :: Show a => a -> IO ()
write = put


-- | Alias of print
writeln :: (Show a) => a -> IO ()
writeln = print

-- | Alias of print
putln :: (Show a) => a -> IO ()
putln = print

-- | maps an IO function in depth N to the given list. Also versions without _ for storing of the returns.
--
-- Again there are also mapM_3, mapM_4 and mapM_5 defined (as well as versions without underscores)
-- 
-- > $ mapM_2 write [[1,2,3,4,5],[1,2]]
-- > 1234512
mapM_2 :: (Monad m) => (a -> m b) -> [[a]] -> m ()
mapM_2 f x = mapM_ (mapM_ f) x
mapM_3 f x = mapM_ (mapM_ (mapM_ f)) x
mapM_4 f x = mapM_ (mapM_ (mapM_ (mapM_ f))) x
mapM_5 f x = mapM_ (mapM_ (mapM_ (mapM_ (mapM_ f)))) x

mapM2 f x = mapM (mapM f) x
mapM3 f x = mapM (mapM (mapM f)) x
mapM4 f x = mapM (mapM (mapM (mapM f))) x
mapM5 f x = mapM (mapM (mapM (mapM (mapM f)))) x


-- | takes a list and returns a random element from that list
-- 
-- > $ rand [1..5]
-- > 5
-- > $ rand "hello there people"
-- > 'l'
rand :: [a] -> IO a
rand xs = do
	i <- getStdRandom (randomR (0,(len xs)-1))
	return (xs !! i)