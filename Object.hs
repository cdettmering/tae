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

module Object where
import qualified Look as L

type ObjectID = String 
data Object = Object {name :: String, desc :: String, objectId :: ObjectID,  pickable :: Bool} deriving Eq

instance L.Look Object where
    getDescription o = desc o
    getName o = name o
    getFull o = (name o) ++ "\n\n" ++ (desc o)

{-
 - Puts the names of the Objects into a list of Strings
-}
names :: [Object] -> [String]
names [] = []
names (x:xs) = name x : names xs
