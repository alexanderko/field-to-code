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
import Unit exposing (Unit)


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


type alias RunningScript =
    ( List Action, Maybe Action, List Action )


type alias Game =
    { player : Unit
    , enemy : Unit
    , effects : List Effect
    , state : GameState
    }


type GameState
    = Pending
    | Loose
    | Win


type EffectIcon
    = HitIcon


type Effect
    = CellEffect EffectIcon Coord


runScript : Script -> RunningScript
runScript script =
    ( [], Nothing, DropList.toValueList script )


newGame : Game
newGame =
    { enemy = Unit ( 1, 2 ) Up
    , player = Unit ( 1, 0 ) Up
    , effects = []
    , state = Pending
    }


type Model
    = Editing Script
    | Run RunningScript Game
    | GameEnd RunningScript Game


init : flags -> ( Model, Cmd Msg )
init _ =
    ( Editing DropList.empty, Cmd.none )


type Msg
    = StartGame
    | Tick
    | AddAction Action
    | Remove ActionItem
    | Edit


scheduleTick : Cmd Msg
scheduleTick =
    Process.sleep theme.duration
        |> perform (always Tick)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( AddAction a, Editing script ) ->
            ( Editing (DropList.push a script), Cmd.none )

        ( Remove item, Editing script ) ->
            ( Editing (DropList.remove item script), Cmd.none )

        ( StartGame, Editing script ) ->
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
                    let
                        finalGame =
                            game
                                |> clearEffects
                                |> looseIfNotWin
                    in
                    ( GameEnd nextScript finalGame, Cmd.none )

                Just action ->
                    let
                        updatedGame =
                            updateGame action game
                    in
                    if game.state == Pending then
                        ( Run nextScript updatedGame, scheduleTick )

                    else
                        ( GameEnd nextScript updatedGame, Cmd.none )

        ( Edit, GameEnd script _ ) ->
            ( edit script, Cmd.none )

        ( _, _ ) ->
            Debug.todo "hadle bad (msg|model) combination"


looseIfNotWin : Game -> Game
looseIfNotWin game =
    if game.state == Win then
        game

    else
        { game | state = Loose }


edit : RunningScript -> Model
edit ( done, action, rest ) =
    let
        actions =
            List.concat
                [ done
                , action |> maybeToList
                , rest
                ]
    in
    Editing (DropList.fromList actions)


maybeToList : Maybe a -> List a
maybeToList =
    Maybe.map List.singleton >> Maybe.withDefault []


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
updateGame action game =
    game
        |> clearEffects
        |> when stateIsPending (applyAction action)


when : (Game -> Bool) -> (Game -> Game) -> Game -> Game
when guard updateFn game =
    if guard game then
        updateFn game

    else
        game


stateIsPending : Game -> Bool
stateIsPending { state } =
    state == Pending


clearEffects : Game -> Game
clearEffects game =
    let
        expectedHit =
            CellEffect HitIcon game.enemy.coord

        hitEnemy =
            List.member expectedHit game.effects

        state =
            if hitEnemy then
                Win

            else
                game.state
    in
    { game | effects = [], state = state }


applyAction : Action -> Game -> Game
applyAction action game =
    case action of
        Turn rotation ->
            { game | player = Unit.turnUnit rotation game.player }

        Step ->
            { game | player = Unit.move game.player }

        Hit ->
            applyPlayerHit game


applyPlayerHit : Game -> Game
applyPlayerHit game =
    let
        effect =
            CellEffect HitIcon (Unit.targetCoord game.player)
    in
    { game | effects = effect :: game.effects }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    case model of
        Editing script ->
            div []
                [ gameView newGame
                , scriptView script
                , hr [] []
                , toolboxView actionToolbox
                , button [ onClick StartGame ] [ text "Start" ]
                ]

        Run script game ->
            div
                []
                [ gameView game
                , runningScriptView script
                ]

        GameEnd script game ->
            div
                []
                [ gameView game
                , runningScriptView script
                , stateView game.state
                , button [ onClick Edit ] [ text "Reset and edit" ]
                ]


stateView : GameState -> Html Msg
stateView state =
    case state of
        Pending ->
            div [] []

        Loose ->
            div [ css [ fontSize (px 32), color (rgb 200 0 0) ] ] [ text "You lost!" ]

        Win ->
            div [ css [ fontSize (px 32), color (rgb 0 200 0) ] ] [ text "You won!" ]


gameView : Game -> Html Msg
gameView { enemy, player, effects } =
    div
        [ css
            [ position relative
            , width (mulCellSize 3)
            , height (mulCellSize 3)
            ]
        ]
        ([ span [ unitCss player ] [ text "🐾" ]
         , span [ unitCss enemy ] [ text "🐲" ]
         ]
            ++ List.map effectView effects
        )


effectView : Effect -> Html Msg
effectView (CellEffect _ coord) =
    span
        [ css
            [ fontSize (px 84)
            , position absolute
            , toCellOffset coord
            ]
        ]
        [ text "⚔️" ]


mulCellSize : Int -> Px
mulCellSize factor =
    theme.cellSize * factor |> toFloat |> px


theme : { cellSize : Int, duration : Float }
theme =
    { cellSize = 100, duration = 1000 }


unitCss : Unit -> Attribute Msg
unitCss unit =
    let
        aggle =
            unit.direction
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
        , toCellOffset unit.coord
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
                |> Maybe.map (runActionView <| css [ borderColor (rgb 0 255 0), borderWidth (px 1), boxShadow3 (px 0) (px 0) (px 4) ])
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
