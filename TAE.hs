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
    Katie Jurek             admin@katiejurek.com
    Chad Dettmering     chad.dettmering@gmail.com
-}

import Data.Maybe
import qualified Data.Map as M
import qualified Data.List as L
import qualified World as W
import qualified Player as P
import qualified IO as I

gameIOLoop :: W.World -> IO()
gameIOLoop w = do 
               putStr (W.output w ++ "\n\n")
               input <- getLine
               let g = gameLoop input w
               gameIOLoop g

gameLoop :: String -> W.World -> W.World
gameLoop s w = I.parse (L.words s) w

main :: IO()
main = let world = W.createWorld in gameIOLoop (W.setOutput world (W.worldString world))
