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
	Katie Jurek        admin@katiejurek.com
-}

module Command where
import Room
import Player
import World

{-
 - Given a RoomID and the World, move the player to the room identified
 - by RoomID
-}
go :: RoomID -> World -> World
           -- Get the room the player is in
go rid w = case (getPlayerRoom w) of
                Just room -> case (isValidExit rid room) of
                                -- If room is a valid exit then set Player location to new room
                                True -> World (Player rid) (wmap w)
                                -- If room is not a valid exit from current room, do not modify World
                                False -> w
                -- If room cannot be found do not modify World
                Nothing -> w
