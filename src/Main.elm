module Main exposing (main)

import Browser
import Coord exposing (Coord)
import Css exposing (..)
import Css.Transitions exposing (transition)
import Direction exposing (Direction(..), Rotation(..))
import DropList
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
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


type Action
    = Turn Rotation
    | Step
    | Hit


actionToolbox : List Action
actionToolbox =
    [ Turn CounterCw, Step, Turn Clockwise, Hit ]


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
    Process.sleep theme.duration
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
updateGame action (Game player) =
    case action of
        Turn rotation ->
            Game (turnUnit rotation player)

        Step ->
            Game (moveUnit player)

        _ ->
            Game player


moveUnit : Unit -> Unit
moveUnit unit =
    { unit | coord = Coord.move unit.direction unit.coord }


turnUnit : Rotation -> Unit -> Unit
turnUnit rotation unit =
    { unit | direction = Direction.turn rotation unit.direction }


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
                , button [ onClick StartGame ] [ text "Start" ]
                ]

        Run script (Game player) ->
            div []
                [ div
                    [ css
                        [ position relative
                        , width (mulCellSize 3)
                        , height (mulCellSize 3)
                        ]
                    ]
                    [ span [ playerCss player ] [ text "🐾" ]
                    ]
                , runningScriptView script
                ]


mulCellSize : Int -> Px
mulCellSize factor =
    theme.cellSize * factor |> toFloat |> px


theme : { cellSize : Int, duration : Float }
theme =
    { cellSize = 100, duration = 1000 }


playerCss : Unit -> Attribute Msg
playerCss player =
    let
        aggle =
            player.direction
                |> Direction.toInt
                |> toFloat
                |> (*) 90.0
    in
    css
        [ fontSize (px 84)
        , transition
            [ Css.Transitions.transform theme.duration
            , Css.Transitions.left theme.duration
            , Css.Transitions.bottom theme.duration
            ]
        , position absolute
        , toCellOffset player.coord
        , transform (rotate (deg aggle))
        ]


toCellOffset : Coord -> Css.Style
toCellOffset ( x, y ) =
    Css.batch
        [ left (mulCellSize x)
        , bottom (mulCellSize y)
        ]


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


runningScriptView : RunningScript -> Html Msg
runningScriptView script =
    let
        ( done, curr, rest ) =
            script
    in
    div [] <|
        List.concat
            [ done
                |> List.map (runActionView <| css [ opacity (num 0.5) ])
            , curr
                |> Maybe.map (runActionView <| css [ borderColor (rgb 0 255 0), borderWidth (px 5) ])
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
            , rest
                |> List.map (runActionView <| css [])
            ]


runActionView : Attribute Msg -> Action -> Html Msg
runActionView styles action =
    styledAction [ styles ]
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
        Turn rotation ->
            case rotation of
                Clockwise ->
                    "↻"

                CounterCw ->
                    "↺"

        Step ->
            "⬆️"

        Hit ->
            "⚔️"
