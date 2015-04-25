
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
    Chad Dettmering     chad.dettmering@gmail.com
    Katie Jurek             admin@katiejurek.com
-}

module IO where
import Command
import World
import Look
import Room
import Person
import qualified Data.List as L

{-
 - Given the player input as a [String], parse the input and execute the command if any.
-}
parse :: [String] -> World -> World
parse ("go":"to":room) w = go (L.intercalate " " room) w
parse ("go":room) w = go (L.intercalate " " room) w
parse ("pick":"up":obj) w = pickup (L.intercalate " " obj) w
parse ("pick":obj) w = pickup (L.intercalate " " obj) w
parse ("look":"at":l) w = look (L.intercalate " " l) w
parse ("look":l) w = look (L.intercalate " " l ) w
parse ("talk":"to":prs) w = talk (L.intercalate " " prs) w
parse ("talk":prs) w = talk (L.intercalate " " prs) w
parse x w = w
