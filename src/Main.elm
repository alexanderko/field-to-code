module Main exposing (main)

import Array exposing (Array)
import Browser
import Css exposing (..)
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
    = TornClockwise
    | TornCounterCw
    | Step
    | Hit


actionToolbox : List Action
actionToolbox =
    [ TornClockwise, TornCounterCw, Step, Hit ]


type alias Script =
    Array RemovableAction


type RemovableAction
    = Present Action
    | Removed


push : Action -> Script -> Script
push action script =
    Array.push (Present action) script


removeAt : Int -> Script -> Script
removeAt index script =
    Array.set index Removed script


toScript : List Action -> Script
toScript =
    List.map Present >> Array.fromList


toList : Script -> List Action
toList script =
    Array.foldr addPresent [] script


toIndexedPresent : Script -> List ( Int, Action )
toIndexedPresent script =
    script
        |> Array.toIndexedList
        |> List.filterMap toMaybeIndexedAction


toMaybeIndexedAction : ( Int, RemovableAction ) -> Maybe ( Int, Action )
toMaybeIndexedAction ( index, removable ) =
    case removable of
        Present action ->
            Just ( index, action )

        Removed ->
            Nothing


addPresent : RemovableAction -> List Action -> List Action
addPresent removable list =
    case removable of
        Present action ->
            action :: list

        Removed ->
            list


type alias Toolbox =
    List Action


type Model
    = Edit Script
    | Run ( List Action, List Action )


init : flags -> ( Model, Cmd Msg )
init _ =
    ( Edit Array.empty, Cmd.none )


type Msg
    = StartGame
    | Tick
    | AddAction Action
    | Remove Int


scheduleTick : Cmd Msg
scheduleTick =
    Process.sleep (2 * 1000)
        |> perform (always Tick)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( AddAction a, Edit script ) ->
            ( Edit (push a script), Cmd.none )

        ( Remove index, Edit script ) ->
            ( Edit (removeAt index script), Cmd.none )

        ( StartGame, Edit script ) ->
            ( Run ( [], toList script ), scheduleTick )

        ( Tick, Run run ) ->
            case run of
                ( done, [] ) ->
                    ( Edit (toScript done), Cmd.none )

                ( done, current :: next ) ->
                    ( Run ( current :: done, next ), scheduleTick )

        ( _, _ ) ->
            Debug.todo "hadle bad (msg|model) combination"


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

        Run _ ->
            Debug.todo "handle runnig game view"


scriptView : Script -> Html Msg
scriptView script =
    script
        |> toIndexedPresent
        |> List.map removableActionView
        |> div []


removableActionView : ( Int, Action ) -> Html Msg
removableActionView ( index, action ) =
    styledAction [ onClick (Remove index) ]
        [ action |> toString |> text ]


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
        TornClockwise ->
            "↻"

        TornCounterCw ->
            "↺"

        Step ->
            "⬆️"

        Hit ->
            "⚔️"
