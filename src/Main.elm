module Main exposing (main)

import Browser
import Css exposing (..)
import DropList
import Html.Styled exposing (..)
import Html.Styled.Events exposing (onClick)
import List
import Process
import Task exposing (perform)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }


type Rotation
    = Clockwise
    | CounterCw


type Action
    = Turn Rotation
    | Step
    | Hit


actionToolbox : List Action
actionToolbox =
    [ Turn Clockwise, Turn CounterCw, Step, Hit ]


type alias Script =
    DropList.DropList Action

type alias ActionItem =
    DropList.Item Action

type alias Toolbox =
    List Action


type alias Unit =
    { coord : Coord
    , direction : Direction
    }


type Direction
    = Up
    | Right
    | Down
    | Left


type alias Coord =
    ( Int, Int )


type alias RunningScript =
    ( List Action, Maybe Action, List Action )


type Game
    = Game Unit


runScript : Script -> RunningScript
runScript script =
    ( [], Nothing, DropList.toValueList script )


newGame : Game
newGame =
    Game { coord = ( 1, 1 ), direction = Up }


type Model
    = Edit Script
    | Run RunningScript Game


init : flags -> ( Model, Cmd Msg )
init _ =
    ( Edit DropList.empty, Cmd.none )


type Msg
    = StartGame
    | Tick
    | AddAction Action
    | Remove ActionItem


scheduleTick : Cmd Msg
scheduleTick =
    Process.sleep (2 * 1000)
        |> perform (always Tick)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( AddAction a, Edit script ) ->
            ( Edit (DropList.push a script), Cmd.none )

        ( Remove item, Edit script ) ->
            ( Edit (DropList.remove item script), Cmd.none )

        ( StartGame, Edit script ) ->
            ( Run (runScript script) newGame, scheduleTick )

        ( Tick, Run script game ) ->
            let
                nextScript =
                    getNext script

                ( _, curr, _ ) =
                    nextScript
            in
            case curr of
                Nothing ->
                    ( edit nextScript, Cmd.none )

                Just action ->
                    ( Run nextScript (updateGame action game), scheduleTick )

        ( _, _ ) ->
            Debug.todo "hadle bad (msg|model) combination"


edit : RunningScript -> Model
edit ( done, _, _ ) =
    Edit <| DropList.fromList <| done


getNext : RunningScript -> RunningScript
getNext script =
    let
        ( done, maybePrev, rest ) =
            script
    in
    case ( maybePrev, rest ) of
        ( Nothing, curr :: next ) ->
            ( [], Just curr, next )

        ( Just prev, curr :: next ) ->
            ( done ++ [ prev ], Just curr, next )

        ( Just prev, [] ) ->
            ( done ++ [ prev ], Nothing, [] )

        ( Nothing, [] ) ->
            script


updateGame : Action -> Game -> Game
updateGame action (Game unit) =
    Game unit


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    case model of
        Edit script ->
            div []
                [ scriptView script
                , hr [] []
                , toolboxView actionToolbox
                ]

        Run _ _ ->
            Debug.todo "handle runnig game view"


scriptView : Script -> Html Msg
scriptView script =
    div []
        (DropList.mapToList removableActionView script)


removableActionView : ActionItem -> Html Msg
removableActionView item =
    styledAction [ onClick (Remove item) ]
        [ item |> DropList.getValue |> toString |> text ]


toolboxView : Toolbox -> Html Msg
toolboxView actions =
    div [] (List.map toolboxActionView actions)


toolboxActionView : Action -> Html Msg
toolboxActionView action =
    styledAction [ onClick (AddAction action) ]
        [ action |> toString |> text ]


styledAction : List (Attribute Msg) -> List (Html Msg) -> Html Msg
styledAction =
    styled button
        [ border3 (px 1) solid (hex "#999")
        , backgroundColor (hex "#eee")
        , borderRadius (px 5)
        , padding (px 4)
        , margin (px 4)
        , fontSize (px 20)
        , cursor pointer
        , hover
            [ borderColor (hex "#240536") ]
        ]


toString : Action -> String
toString action =
    case action of
        Turn rotation  ->
            case rotation of
                Clockwise ->
                    "↻"

                CounterCw ->
                    "↺"

        Step ->
            "⬆️"

        Hit ->
            "⚔️"
