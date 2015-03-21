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
import qualified Object as O
import Room

data World = World {player :: P.Player, wmap :: M.Map RoomID Room}

createWorld :: World
createWorld = World
              {
                  player = P.Player "Dirt Trail",
                  wmap = M.fromList
                        [
                           ("Dirt Trail", Room {title = "Dirt Trail", desc = "Dirt trail leading to castle", exits = ["Drawbridge"], roomId = "Dirt Trail", objects = [O.Object {O.name = "stick", O.desc = "stick from a tree"}]}),
                           ("Drawbridge", Room {title = "Drawbridge", desc = "Drawbridge over moat", exits = ["Courtyard", "Dirt Trail"], roomId = "Drawbridge", objects = []}),
                           ("Courtyard", Room {title = "Courtyard", desc = "Courtyard", exits = ["Drawbridge", "Grand Hall", "SouthWest Tower", "SouthEast Tower", "West Path", "East Path", "Graveyard"], roomId = "Courtyard", objects = []}),
                           ("Grand Hall", Room {title = "Grand Hall", desc = "Grand Hall", exits = ["Kitchen", "Bedroom", "Courtyard"], roomId = "Grand Hall", objects = []}),
                           ("Kitchen", Room {title = "Kitchen", desc = "Kitchen", exits = ["Grand Hall", "West Path"], roomId = "Kitchen", objects = []}),
                           ("Bedroom", Room {title = "Bedroom", desc = "Bedroom", exits = ["Grand Hall", "East Path"], roomId = "Bedroom", objects = []}),
                           ("East Path", Room {title = "East Path", desc = "East Path", exits = ["Courtyard", "Bedroom", "SouthEast Tower", "Graveyard"], roomId = "East Path", objects = []}),
                           ("West Path", Room {title = "West Path", desc = "West Path", exits = ["Courtyard", "Kitchen", "SouthWest Tower"], roomId = "West Path", objects = []}),
                           ("SouthWest Tower", Room {title = "SouthWest Tower", desc = "SouthWest Tower", exits = ["Courtyard", "West Path"], roomId = "SouthWest Tower", objects = []}),
                           ("SouthEast Tower", Room {title = "SouthEast Tower", desc = "SouthEast Tower", exits = ["Courtyard", "East Path"], roomId = "SouthEast Tower", objects = []}),
                           ("Graveyard", Room {title = "Graveyard", desc = "Graveyard", exits = ["Courtyard", "East Path"], roomId = "Graveyard", objects = []})
                        ]
              }

{-
 - Given a RoomID and the World, return the Room that matches the RoomID, if any
-}
getRoom :: RoomID -> World -> Maybe Room
getRoom r World {player = p, wmap = w}  = M.lookup r w

{-
 - Get the Room the Player is currently in
-}
getPlayerRoom :: World -> Maybe Room
getPlayerRoom w = getRoom (P.loc (player w)) w 
