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
import Room
import Player
import World

go :: RoomID -> World -> World
go rid w = case (getPlayerRoom w) of
                Just room -> case (isValidExit rid room) of
                                True -> World (Player rid) (wmap w)
                                False -> w
                Nothing -> w
