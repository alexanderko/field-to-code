module Direction exposing (Direction(..), Rotation(..), toString, turn)

import Array exposing (Array)


type Direction
    = Up
    | Right
    | Down
    | Left


toString : Direction -> String
toString direction =
    case direction of
        Up ->
            "Up"

        Right ->
            "Right"

        Down ->
            "Down"

        Left ->
            "Left"


type Rotation
    = Clockwise
    | CounterCw


toInt : Direction -> Int
toInt d =
    case d of
        Up ->
            0

        Right ->
            1

        Down ->
            2

        Left ->
            3


directions : Array Direction
directions =
    Array.fromList [ Up, Right, Down, Left ]


rToInt : Rotation -> Int
rToInt r =
    case r of
        Clockwise ->
            1

        CounterCw ->
            -1


turn : Rotation -> Direction -> Direction
turn r d =
    let
        currI =
            toInt d

        nextI =
            currI + rToInt r |> modBy 4

        nd =
            Array.get nextI directions
    in
    case nd of
        Just direction ->
            direction

        Nothing ->
            Debug.todo "something wrong with turn code"
