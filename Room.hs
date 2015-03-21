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
import qualified Data.List as L

type RoomID = String
data Room = Room {title :: String, desc :: String, exits :: [RoomID], roomId :: RoomID, objects :: [O.Object]}

{-
 - Gives a human readable String of the Room
-}
roomString :: Room -> String
roomString r = (title r) ++ "\n\n" ++ (desc r) ++ (exitsString (exits r)) ++ (objectsString (objects r))

{-
 - Gives a human readable String representation of the exits in the Room
-}
exitsString :: [RoomID] -> String
exitsString [] = ""
exitsString exts = "\nYou can go to: " ++ (listToString ", " exts)

{-
 - Gives a human readable String representation of the objects in the Room
-}
objectsString :: [O.Object] -> String
objectsString [] = ""
objectsString objs = "\nYou can see: " ++ (listToString ", " (O.names objs))

{-
 - Given a RoomID and a Room, checks if RoomID is an exit of Room
-}
isValidExit :: RoomID -> Room -> Bool
isValidExit rid r = elem rid (exits r)

{-
 - Retrieves an Object from the Room given the ObjectID, if it exists
-}
getObject :: O.ObjectID -> Room -> Maybe O.Object
getObject oid r = L.find (\x -> if O.objectId x == oid then True else False) (objects r)

{-
 - Removes the Object from the Room that corresponds to the given ObjectID
-}
removeObject :: O.ObjectID -> Room -> Room
removeObject oid r = case (getObject oid r) of
                        Just object -> Room (title r) (desc r) (exits r) (roomId r) (L.delete object (objects r))
                        Nothing -> r
