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

Copyright (c) 2015 Katie Jurek, Chad Dettmering

Authors:
    Katie Jurek             admin@katiejurek.com
    Chad Dettmering     chad.dettmering@gmail.com
-}

module World where
import qualified Data.Map as M
import qualified Player as P
import qualified Object as O
import qualified Person as Prs
import Room

type WorldMap = M.Map RoomID Room
data World = World {player :: P.Player, wmap :: WorldMap}

createWorld :: World
createWorld = World
              {
                  player = P.Player "Dirt Trail" [],
                  wmap = M.fromList
                        [
                           ("Dirt Trail", Room 
                                            {
                                                title = "Dirt Trail", 
                                                desc = "Dirt trail leading to castle", 
                                                exits = ["Drawbridge"], 
                                                roomId = "Dirt Trail", 
                                                objects = [O.Object {O.name = "stick", O.desc = "stick from a tree", O.objectId = "stick", O.pickable = True}],
                                                people = []
                                            }
                           ),
                           ("Drawbridge", Room 
                                            {
                                                title = "Drawbridge", 
                                                desc = "Drawbridge over moat", 
                                                exits = ["Courtyard", "Dirt Trail"], 
                                                roomId = "Drawbridge", 
                                                objects = [],
                                                people = []
                                            }
                           ),
                           ("Courtyard", Room 
                                            {
                                                title = "Courtyard", 
                                                desc = "Courtyard", 
                                                exits = ["Drawbridge", "Grand Hall", "SouthWest Tower", "SouthEast Tower", "West Path", "East Path", "Graveyard"], 
                                                roomId = "Courtyard", 
                                                objects = [],
                                                people = []
                                            }
                           ),
                           ("Grand Hall", Room 
                                            {
                                                title = "Grand Hall", 
                                                desc = "Grand Hall", 
                                                exits = ["Kitchen", "Bedroom", "Courtyard"], 
                                                roomId = "Grand Hall", 
                                                objects = [],
                                                people = []
                                            }
                           ),
                           ("Kitchen", Room 
                                            {
                                                title = "Kitchen", 
                                                desc = "Kitchen", 
                                                exits = ["Grand Hall", "West Path"], 
                                                roomId = "Kitchen", 
                                                objects = [],
                                                people = [Prs.Person {Prs.name = "Zombie Maid", Prs.desc = "A zombie maid. She looks pale.", Prs.personId = "Zombie Maid"}]
                                            }
                           ),
                           ("Bedroom", Room 
                                            {
                                                title = "Bedroom", 
                                                desc = "Bedroom", 
                                                exits = ["Grand Hall", "East Path"], 
                                                roomId = "Bedroom", 
                                                objects = [],
                                                people = []
                                            }
                           ),
                           ("East Path", Room 
                                            {
                                                title = "East Path", 
                                                desc = "East Path", 
                                                exits = ["Courtyard", "Bedroom", "SouthEast Tower", "Graveyard"], 
                                                roomId = "East Path", 
                                                objects = [],
                                                people = []
                                            }
                           ),
                           ("West Path", Room 
                                            {
                                                title = "West Path", 
                                                desc = "West Path", 
                                                exits = ["Courtyard", "Kitchen", "SouthWest Tower"], 
                                                roomId = "West Path", 
                                                objects = [],
                                                people = []
                                            }
                           ),
                           ("SouthWest Tower", Room 
                                            {
                                                title = "SouthWest Tower", 
                                                desc = "SouthWest Tower", 
                                                exits = ["Courtyard", "West Path"], 
                                                roomId = "SouthWest Tower", 
                                                objects = [],
                                                people = []
                                            }
                           ),
                           ("SouthEast Tower", Room 
                                            {
                                                title = "SouthEast Tower", 
                                                desc = "SouthEast Tower", 
                                                exits = ["Courtyard", "East Path"], 
                                                roomId = "SouthEast Tower", 
                                                objects = [],
                                                people = []
                                            }
                           ),
                           ("Graveyard", Room 
                                            {
                                                title = "Graveyard", 
                                                desc = "Graveyard", 
                                                exits = ["Courtyard", "East Path"], 
                                                roomId = "Graveyard", 
                                                objects = [],
                                                people = []
                                            }
                            )
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

{-
 - Replaces the Room corresponding to the given RoomId with the new given
 - Room in the WorldMap
-}
replaceRoom :: RoomID -> Room -> WorldMap -> WorldMap
replaceRoom rid r w = M.adjust (\x -> r) rid w

{-
 - Transfers the Object corresponding to the given ObjectID from the Room corresponding
 - to the given RoomID to the Player
-}
transferObjectFromRoomToPlayer :: O.ObjectID -> RoomID -> World -> World
                                       -- Get the Room given the RoomID
transferObjectFromRoomToPlayer o r w = case (getRoom r w) of
                                            -- Get the Object in the Room given the ObjectID
                                            Just room -> case (getObject o room) of
                                                            -- Add the Object to the Player inventory. Remove the Object from the Room. Replace the old Room with the new Room
                                                            -- where the Object is missing.
                                                            Just object -> World (P.addObject object (player w)) (replaceRoom r (removeObject (O.objectId object) room) (wmap w))
                                                            -- Couldn't find corresponding Object in Room, do nothing.
                                                            Nothing -> w
                                            -- Couldn't find corresponding Room, do nothing.
                                            Nothing -> w

{-
 - Gives a human readable String representation of the Player surroundings in the World.
-}
worldString :: World -> String
worldString w = case (getPlayerRoom w) of
                    Just room -> (roomString room) ++ (P.inventoryString (player w))
                    Nothing -> ""
