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

import Data.Maybe
import qualified Data.Map as M
import qualified World as W
import qualified IO as I
import qualified IOUtils as IU

gameIOLoop :: W.World -> IO()
gameIOLoop w = case (W.getPlayerRoom w) of
                   Just room -> do 
                                   print room
                                   input <- getLine
                                   let g = gameLoop input w
                                   gameIOLoop g
                   Nothing -> error "Room doesn't exist!"

gameLoop :: String -> W.World -> W.World
gameLoop s w = I.parse (IU.splitOnWhiteSpace s) w

main :: IO()
main = let world = W.createWorld in gameIOLoop world
