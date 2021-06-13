module CoordTest exposing (..)

import Coord exposing (Coord)
import Direction exposing (Direction(..))
import Expect
import Test exposing (..)
import Fuzz exposing (Fuzzer)

axis : Fuzzer Int
axis = 
    Fuzz.intRange 0 2


coord : Fuzzer (Int, Int)
coord = 
    Fuzz.tuple (axis, axis)


suite : Test
suite =
    describe "Coord move"
        [ fuzz coord "down" <|
            \(x, y) ->
                ( x, y )
                    |> Coord.move Down
                    |> Expect.equal ( x, y - 1 )
        , fuzz coord "left" <|
            \(x, y) ->
                ( x, y )
                    |> Coord.move Left
                    |> Expect.equal ( x - 1, y )
        , fuzz coord "up" <|
            \(x, y) ->
                ( x, y )
                    |> Coord.move Up
                    |> Expect.equal ( x, y + 1 )
        , fuzz coord "right" <|
            \(x, y) ->
                ( x, y )
                    |> Coord.move Right
                    |> Expect.equal ( x + 1, y )
        ]
