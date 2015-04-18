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
    Chad Dettmering     chad.dettmering@gmail.com
	Katie Jurek             admin@katiejurek.com
-}

module Person where
import qualified Look as L

type PersonID = String 
data Person = Person {name :: String, desc :: String, personId :: PersonID} deriving Eq

instance L.Look Person where
    getDescription p = desc p
    getName p = name p
    getFull p = (name p) ++ "\n\n" ++ (desc p)

{-
 - Puts the names of the Persons into a list of Strings
-}
names :: [Person] -> [String]
names [] = []
names (x:xs) = name x: names xs
