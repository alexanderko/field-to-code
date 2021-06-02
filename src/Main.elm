module Main exposing (main)

import Browser
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
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
    List Action


type alias Toolbox =
    List Action


type alias Model =
    { script : Script
    , run : ( Script, Script )
    }


stepAndHit : Script
stepAndHit =
    [ Step, Hit ]


init : flags -> ( Model, Cmd Msg )
init _ =
    ( Model stepAndHit ( [], stepAndHit ), Cmd.none )


type Msg
    = StartGame
    | Tick


scheduleTick : Cmd Msg
scheduleTick =
    Process.sleep (2 * 1000)
        |> perform (always Tick)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartGame ->
            ( { model | run = ( [], model.script ) }, scheduleTick )

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
        [ actionsView (model.run |> Tuple.first)
        , hr [] []
        , actionsView (model.run |> Tuple.second)
        , hr [] []
        , actionsView model.script
        , hr [] []
        , toolboxView actionToolbox
        ]


actionsView : Script -> Html Msg
actionsView actions =
    div [] (List.map actionView actions)


toolboxView : Toolbox -> Html Msg
toolboxView actions =
    div [] (List.map actionView actions)


actionView : Action -> Html Msg
actionView action =
    span
        [ css
            [ border3 (px 1) solid (hex "#999")
            , backgroundColor (hex "#eee")
            , borderRadius (px 5)
            , padding (px 4)
            , margin (px 4)
            , fontSize (px 20)
            , cursor pointer
            , hover
                [ borderColor (hex "#240536")
                ]
            ]
        ]
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
