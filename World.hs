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
                  player = P.Player "Start",
                  wmap = M.fromList
                        [
                           ("Start", Room {title = "Start", desc = "Starting room", exits = ["End"], roomId = "Start"}),
                           ("End", Room {title = "End", desc = "Ending room", exits = ["Start"], roomId = "End"})
                        ]
              }

getRoom :: RoomID -> World -> Maybe Room
getRoom r World {player = p, wmap = w}  = M.lookup r w

getPlayerRoom :: World -> Maybe Room
getPlayerRoom w = getRoom (P.loc (player w)) w 
