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

module Room where
import IOUtils
import qualified Object as O

type RoomID = String
data Room = Room {title :: String, desc :: String, exits :: [RoomID], roomId :: RoomID, objects :: [O.Object]}

instance Show Room where
    show (Room {title = t, desc = d, exits = e, roomId = r, objects = o}) = t ++ "\n\n" ++ d ++ "\n" ++ "You can go to: " ++ (listToString ", " e) ++ objectsString o

objectsString :: [O.Object] -> String
objectsString [] = ""
objectsString objs = "\nYou can see: " ++ (listToString ", " (O.names objs))

{-
 - Given a RoomID and a Room, checks if RoomID is an exit of Room
-}
isValidExit :: RoomID -> Room -> Bool
isValidExit rid r = elem rid (exits r)
