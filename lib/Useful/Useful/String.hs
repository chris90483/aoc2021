-- | String operations, part of the "Useful" module.
module Useful.String where

import Useful.General

-- | Strips whitespace from either side of a string.
--
-- > $ strip " \v\r  asdsadasds   \r\n"
-- > "asdsadasds"	
strip :: String -> String
strip = stripr . stripl

-- | Strips whitespace from the right of a string
--
-- > $ stripr "  asdioamlksd   \n\n"
-- > "  asdioamlksd"
stripr :: String -> String
stripr [] = []
stripr x
	|(last x) ? whiteSpaceChars = stripr (init x)
	|otherwise = x

-- | Strips whitespace from the left of a string
--
-- > $ stripl " \n\n  askdjnasdnaskd"
-- > "askdjnasdnaskd"
stripl :: String -> String
stripl [] = []
stripl (x:xs)
	|x ? whiteSpaceChars = stripl xs
	|otherwise = (x:xs)

-- | Alias of 'strip'
chomp :: String -> String
chomp = stripr

-- | List of whitespace characters: 
--
-- > $whiteSpaceChars
-- > [' ','\r','\n','\t','\f','\v']
whiteSpaceChars :: [Char]
whiteSpaceChars = [' ','\r','\n','\t','\f','\v']