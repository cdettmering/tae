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

Copyright (c) 2015 Katie Jurek, Chad Dettmering

Authors:
    Katie Jurek             admin@katiejurek.com
    Chad Dettmering     chad.dettmering@gmail.com
-}

module Room where
import qualified Object as O
import qualified Person as P
import qualified Look as LO
import qualified Data.List as L
import qualified Data.Maybe as M

type RoomID = String
data Room = Room {title :: String, desc :: String, exits :: [RoomID], roomId :: RoomID, objects :: [O.Object], people :: [P.Person]}

{-
 - Gives a human readable String of the Room
-}
roomString :: Room -> String
roomString r = (title r) ++ "\n\n" ++ (desc r) ++ (exitsString (exits r)) ++ (objectsString (objects r)) ++ (peopleString (people r))

{-
 - Gives a human readable String representation of the exits in the Room
-}
exitsString :: [RoomID] -> String
exitsString [] = ""
exitsString exts = "\nYou can go to: " ++ (L.intercalate ", " exts)

{-
 - Gives a human readable String representation of the objects in the Room
-}
objectsString :: [O.Object] -> String
objectsString [] = ""
objectsString objs = "\nYou can see: " ++ (L.intercalate ", " (O.names objs))

{-
 - Gives a human readable String representation of the people in the Room
-}
peopleString :: [P.Person] -> String
peopleString [] = ""
peopleString ppl = "\nYou can see: " ++ (L.intercalate ", " (P.names ppl))

{-
 - Given a RoomID and a Room, checks if RoomID is an exit of Room
-}
isValidExit :: RoomID -> Room -> Bool
isValidExit rid r = elem rid (exits r)

{-
 - Retrieves an Object from the Room given the ObjectID, if it exists
-}
getObject :: O.ObjectID -> Room -> M.Maybe O.Object
getObject oid r = L.find (\x -> O.objectId x == oid) (objects r)

{-
 - Retrieves a Person from the Room given the PersonID, if it exists
-}
getPerson :: P.PersonID -> Room -> M.Maybe P.Person
getPerson pid r = L.find (\x -> P.personId x == pid) (people r)

{-
 - Removes the Object from the Room that corresponds to the given ObjectID
-}
removeObject :: O.ObjectID -> Room -> Room
removeObject oid r = M.fromMaybe r (do
                                       object <- getObject oid r
                                       Just (Room (title r) (desc r) (exits r) (roomId r) (L.delete object (objects r)) (people r))
                                   )
