
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

Copyright (c) 2015 Chad Dettmering

Authors:
    Chad Dettmering     chad.dettmering@gmail.com
-}

module IO where
import qualified Data.Text as T 
import qualified Data.List.Split as S
import Command
import World

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

listToString :: [String] -> String
listToString [] = ""
listToString (x:[]) = x
listToString (x:xs) = x ++ ", " ++ listToString xs

{-
 - Given the player input as a [String], parse the input and execute the command if any.
-}
parse :: [String] -> World -> World
parse ("go":"to":room) w = go (listToString room) w
parse ("go":room) w = go (listToString room) w
parse x w = w
