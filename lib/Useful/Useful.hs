{- |
		   
These are a selection of useful haskell functions which I've written to speed general programming.

Several people suggested I re-name this module \"Useless\" and I can see where they are coming from. Don't expect any of this code to save the world. Lots of it is largely redundant (alises, small functions) and based on personal preference, but don't tell me there is never a time you've written "!=" by mistake instead of "/=", or thought things would be easier if you could set up a quick dictionary, or anything else like this.
It is split into 5 sections so as to allow you to import just one if you think the whole lot will clutter up the namepsace.

	* "Useful.General" 		- General shorthands and the most commonly used.
	
	* "Useful.Dictionary"	- A lightweight dictionary implementation using Data.Map
	
	* "Useful.List"			- Some general purpose list functions
	
	* "Useful.String"		- A few string functions which don't make sense over all lists

	* "Useful.IO"			- Some shorthand and general purpose IO functions

For any issues or queries with the code you can contact me at contact@theorangeduck.com
	
-}
module Useful(module Useful.General, module Useful.Dictionary, module Useful.List, module Useful.String, module Useful.IO) where

import Useful.General
import Useful.Dictionary
import Useful.List
import Useful.String
import Useful.IO
