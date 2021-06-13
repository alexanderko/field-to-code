module Coord exposing (Coord, move)

import Direction exposing (Direction(..))
import Tuple exposing (mapBoth, mapFirst, mapSecond)


type alias Coord =
    ( Int, Int )


move : Direction -> Coord -> Coord
move direction =
    case direction of
        Up ->
            mapSecond inc

        Right ->
            mapFirst inc

        Down ->
            mapSecond dec

        Left ->
            mapFirst dec


inc : Int -> Int
inc n =
    n + 1


dec : Int -> Int
dec n =
    n - 1
