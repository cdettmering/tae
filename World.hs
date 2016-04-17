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
import qualified Data.Maybe as MA
import qualified Player as P
import qualified Object as O
import qualified Person as Prs
import Room

type WorldMap = M.Map RoomID Room
data World = World {player :: P.Player, wmap :: WorldMap, output :: String}

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
                                                people = [Prs.Person {Prs.name = "Wolf", Prs.desc = "A tattered-looking, gray wolf.", Prs.personId = "Wolf", Prs.dialogue = "He looks at you through narrowed eyes, growling under his breath."}]
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
                                                people = [Prs.Person {Prs.name = "Zombie Maid", Prs.desc = "A zombie maid. She looks pale.", Prs.personId = "Zombie Maid", Prs.dialogue = "She turns to you slowly. \"Are you new here, dearrr?\" she says with an unsettling, lopsided grin."}]
                                            }
                           ),
                           ("Bedroom", Room 
                                            {
                                                title = "Bedroom", 
                                                desc = "Bedroom", 
                                                exits = ["Grand Hall", "East Path"], 
                                                roomId = "Bedroom", 
                                                objects = [O.Object {O.name = "hairbrush", O.desc = "a hairbrush", O.objectId = "hairbrush", O.pickable = True}],
                                                people = [Prs.Person {Prs.name = "Girl", Prs.desc = "A girl sitting down in a chair before her mirror, looking fretful.", Prs.personId = "Girl", Prs.dialogue = "\"I need to get this knot out before the ball. Can you help me?\""}]
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
                                                people = [Prs.Person {Prs.name = "Priest", Prs.desc = "A priest looking out the window.", Prs.personId = "Priest", Prs.dialogue = "The priest, though seeming hostile in his body language, stares at you with an eerie calmness."}]
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
                                                people = [Prs.Person {Prs.name = "Pastor", Prs.desc = "A somber pastor. His head is bowed as he looks upon a particularly grand tombstone.", Prs.personId = "Pastor", Prs.dialogue = "Not even glancing at you, he only whispers repeatedly, \"Shame, shame, shame...\""}]
                                            }
                            )
                        ],
                output = ""
              }

{-
 - Given a RoomID and the World, return the Room that matches the RoomID, if any
-}
getRoom :: RoomID -> World -> Maybe Room
getRoom r World {player = p, wmap = w, output = o}  = M.lookup r w

{-
 - Get the Room the Player is currently in
-}
getPlayerRoom :: World -> Room
-- Player should always be in  a room so this is acceptable
getPlayerRoom w = MA.fromJust (getRoom (P.loc (player w)) w)

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
transferObjectFromRoomToPlayer o r w = MA.fromMaybe (failedTransfer w) (do
                                                                         room <- getRoom r w
                                                                         object <- getObject o room
                                                                         Just (World (P.addObject object (player w)) (replaceRoom r (removeObject (O.objectId object) room) (wmap w)) (output w))
                                                                       )

{-
 - Transfer failed: tell user they can't pick the item up.
-}
failedTransfer :: World -> World
failedTransfer w = setOutput w ("You can't pick that up.")

{-
 - Gives a human readable String representation of the Player surroundings in the World.
-}
worldString :: World -> String
worldString w = let room = getPlayerRoom w in
                    (roomString room) ++ (P.inventoryString (player w))

{-
 - Adds a String to the current output String to display to the Player in the World.
-}
addToOutput :: World -> String -> World
addToOutput w "" = w
addToOutput World {player = p, wmap = w, output = o} s = World p w (o ++ s)

{-
 - Resets the World's output String to the empty String.
-}
resetOutput :: World -> World
resetOutput World {player = p, wmap = w, output = o} = World p w ""

{-
 - Sets the World's output String to the given String (by resetting to "" and then adding the String to the output).
-}
setOutput :: World -> String -> World
setOutput w s = addToOutput (resetOutput w) s
