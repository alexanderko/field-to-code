module Main exposing (main)

import Array exposing (Array, push, toList)
import Browser
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import List
import Process
import Task exposing (perform)
import Tuple


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
    Array Action


type alias Toolbox =
    List Action


type alias Model =
    { script : Script
    , run : ( List Action, List Action )
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( Model Array.empty ( [], [] ), Cmd.none )


type Msg
    = StartGame
    | Tick
    | AddAction Action


scheduleTick : Cmd Msg
scheduleTick =
    Process.sleep (2 * 1000)
        |> perform (always Tick)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddAction a ->
            ( { model | script = push a model.script }, Cmd.none )

        StartGame ->
            ( { model | run = ( [], toList model.script ) }, scheduleTick )

        Tick ->
            case model.run of
                ( _, [] ) ->
                    ( model, Cmd.none )

                ( done, current :: next ) ->
                    let
                        run =
                            ( current :: done, next )
                    in
                    ( { model | run = run }, scheduleTick )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ actionsView model.script
        , hr [] []
        , toolboxView actionToolbox
        ]


actionsView : Script -> Html Msg
actionsView actions =
    div [] (List.map (actionView []) (toList actions))


toolboxView : Toolbox -> Html Msg
toolboxView actions =
    div [] (List.map (actionView [ onClick << AddAction ] ) actions)


actionView : List (Action -> Attribute Msg) -> Action -> Html Msg
actionView attrs action =
    button
        (css
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
            :: List.map ((|>) action) attrs
        )
        [ action |> toString |> text ]


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
