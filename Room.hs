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
    Katie Jurek    admin@katiejurek.com
-}

module Room where
type RoomID = String
data Room = Room {title :: String, desc :: String, exits :: [RoomID], roomId :: RoomID}

instance Show Room where
    show (Room {title = t, desc = d, exits = e, roomId = r}) = t ++ "\n\n" ++ d