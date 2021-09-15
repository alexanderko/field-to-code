module Unit exposing (..)

import Coord exposing (Coord)
import Direction exposing (Direction, Rotation)


type alias Unit =
    { coord : Coord
    , direction : Direction
    }


turnUnit : Rotation -> Unit -> Unit
turnUnit rotation unit =
    { unit | direction = Direction.turn rotation unit.direction }


targetCoord : Unit -> Coord
targetCoord unit =
    Coord.move unit.direction unit.coord


move : Unit -> Unit
move unit =
    { unit | coord = targetCoord unit }
