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

Copyright (c) 2015 Katie Jurek

Authors:
    Katie Jurek         admin@katiejurek.com
    Chad Dettmering     chad.dettmering@gmail.com
-}

module World where
import qualified Data.Map as M
import qualified Player as P
import Room

data World = World {player :: P.Player, wmap :: M.Map RoomID Room}

createWorld :: World
createWorld = World
              {
                  player = P.Player "Dirt Trail",
                  wmap = M.fromList
                        [
                           ("Dirt Trail", Room {title = "Dirt Trail", desc = "Dirt trail leading to castle", exits = ["Drawbridge"], roomId = "Dirt Trail"}),
                           ("Drawbridge", Room {title = "Drawbridge", desc = "Drawbridge over moat", exits = ["Courtyard", "Dirt Trail"], roomId = "Drawbridge"}),
                           ("Courtyard", Room {title = "Courtyard", desc = "Courtyard", exits = ["Drawbridge", "Grand Hall", "SouthWest Tower", "SouthEast Tower", "West Path", "East Path", "Graveyard"], roomId = "Courtyard"}),
                           ("Grand Hall", Room {title = "Grand Hall", desc = "Grand Hall", exits = ["Kitchen", "Bedroom", "Courtyard"], roomId = "Grand Hall"}),
                           ("Kitchen", Room {title = "Kitchen", desc = "Kitchen", exits = ["Grand Hall", "West Path"], roomId = "Kitchen"}),
                           ("Bedroom", Room {title = "Bedroom", desc = "Bedroom", exits = ["Grand Hall", "East Path"], roomId = "Bedroom"}),
                           ("East Path", Room {title = "East Path", desc = "East Path", exits = ["Courtyard", "Bedroom", "SouthEast Tower", "Graveyard"], roomId = "East Path"}),
                           ("West Path", Room {title = "West Path", desc = "West Path", exits = ["Courtyard", "Kitchen", "SouthWest Tower"], roomId = "West Path"}),
                           ("SouthWest Tower", Room {title = "SouthWest Tower", desc = "SouthWest Tower", exits = ["Courtyard", "West Path"], roomId = "SouthWest Tower"}),
                           ("SouthEast Tower", Room {title = "SouthEast Tower", desc = "SouthEast Tower", exits = ["Courtyard", "East Path"], roomId = "SouthEast Tower"})
                           ("Graveyard", Room {title = "Graveyard", desc = "Graveyard", exits = ["Courtyard", "East Path"], roomId = "Graveyard"})
                        ]
              }

getRoom :: RoomID -> World -> Maybe Room
getRoom r World {player = p, wmap = w}  = M.lookup r w

getPlayerRoom :: World -> Maybe Room
getPlayerRoom w = getRoom (P.loc (player w)) w 
