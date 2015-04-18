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
    Chad Dettmering    chad.dettmering@gmail.com
-}

module Player where
import qualified Room as R
import qualified Object as O
import qualified Data.List as L

data Player = Player {loc :: R.RoomID, inv :: [O.Object]}

{-
 - Puts an Object into the Player inventory
-}
addObject :: O.Object -> Player -> Player
addObject obj p = Player (loc p) (obj : (inv p))

{-
 - Gives a human readable String representation of the Player inventory
-}
inventoryString :: Player -> String
inventoryString (Player _ []) = ""
inventoryString (Player _ objs) = "\nYou are carrying: " ++ (L.intercalate ", " (O.names objs))
