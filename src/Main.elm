module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (action)
import Process
import Task exposing (perform)
import Tuple exposing (first)
import Tuple exposing (second)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
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
        , actionsView actionToolbox
        ]


actionsView : Script -> Html Msg
actionsView actions =
    ul [] (List.map actionView actions)


actionView : Action -> Html Msg
actionView action =
    li [] [ action |> toString |> text ]


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
