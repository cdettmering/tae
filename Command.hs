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

{-
 - Given a RoomID and the World, move the player to the room identified
 - by RoomID
-}
go :: R.RoomID -> W.World -> W.World
           -- Get the room the player is in
go rid w = case (W.getPlayerRoom w) of
                Just room -> case (R.isValidExit rid room) of
                                -- If room is a valid exit then set Player location to new room
                                True -> W.World (P.Player rid (P.inv (W.player w ))) (W.wmap w)
                                -- If room is not a valid exit from current room, do not modify World
                                False -> w
                -- If room cannot be found do not modify World
                Nothing -> w

{-
 - Given an ObjectID and the World, pick up the Object and give it to the Player.
 - This removes the Object from the current Room and puts it into the Player inventory.
-}
pickup :: O.ObjectID -> W.World -> W.World
pickup oid w = case (W.getPlayerRoom w) of
                Just room -> case (R.getObject oid room) of
                                Just object -> W.transferObjectFromRoomToPlayer oid (R.roomId room) w
                                Nothing -> w
                Nothing -> w