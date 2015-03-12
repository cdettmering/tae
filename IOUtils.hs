{-
This file is part of TAE.

TAE is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

TAE is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with TAE.  If not, see <http://www.gnu.org/licenses/>.

Copyright (c) 2015 Chad Dettmering, Katie Jurek

Authors:
    Chad Dettmering    chad.dettmering@gmail.com
	Katie Jurek        admin@katiejurek.com
-}

module IOUtils where
import qualified Data.Text as T 
import qualified Data.List.Split as S

{-
 - Checks if Char is a space
-}
isSpace :: Char -> Bool
isSpace c = c == ' '

{-
 - Strips out all whitespace from String
-}
stripWhiteSpace :: String -> String
stripWhiteSpace t = T.unpack (T.filter (\x -> not (isSpace x)) (T.pack t))

{-
 - Turns a white space separated String into a list of Strings
 - Example: "This is a sentence" -> ["This", "is", "a", "sentence"]
-}
splitOnWhiteSpace :: String -> [String]
splitOnWhiteSpace s = S.splitOn " " s

listToString :: String -> [String] -> String
listToString c [] = ""
listToString c (x:[]) = x
listToString c (x:xs) = x ++ c ++ (listToString c xs)
