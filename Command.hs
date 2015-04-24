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
	Katie Jurek            admin@katiejurek.com
-}

module Command where
import qualified Room as R
import qualified Player as P
import qualified World as W
import qualified Object as O
import qualified Look as L
import qualified Data.Maybe as M

{-
 - Given a RoomID and the World, move the player to the room identified
 - by RoomID
-}
go :: R.RoomID -> W.World -> W.World
           -- Get the room the player is in
go rid w = let room = W.getPlayerRoom w in
               case (R.isValidExit rid room) of
                    -- If room is a valid exit then set Player location to new room
                    True -> let world = (W.World (P.Player rid (P.inv (W.player w ))) (W.wmap w) (W.output w)) in W.setOutput world (W.worldString world)
                    -- If room is not a valid exit from current room, do not modify World
                    False -> w

{-
 - Given an ObjectID and the World, pick up the Object and give it to the Player.
 - This removes the Object from the current Room and puts it into the Player inventory.
-}
pickup :: O.ObjectID -> W.World -> W.World
pickup oid w = M.fromMaybe (W.failedTransfer w) (do
                                                    let room = W.getPlayerRoom w
                                                    object <- R.getObject oid room
                                                    let world = W.transferObjectFromRoomToPlayer oid (R.roomId room) w
                                                    Just (W.setOutput world (W.worldString world))
                                                )

{-
 - Given an unkown ID this attempts to look at something in the current room.
 - First an object is located, if the object does not exist, then a person
 - is located. If neither exist then an error message is output.
-}
look :: String -> W.World -> W.World
look l w = let room = W.getPlayerRoom w in 
                          case (R.getObject l room) of
                              Just object -> W.setOutput w (L.getDescription object)
                              Nothing -> case (R.getPerson l room) of
                                             Just person -> W.setOutput w (L.getDescription person)
                                             Nothing -> W.setOutput w ("You don't see that here.")
