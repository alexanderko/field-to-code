module Game exposing (..)

import Coord exposing (Coord)
import Direction exposing (Rotation)
import Unit exposing (Unit)


type Action
    = Turn Rotation
    | Step
    | Hit


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


buildNewGame : Unit -> Unit -> Game
buildNewGame enemy player =
    { enemy = enemy
    , player = player
    , effects = []
    , state = Pending
    }


looseIfNotWin : Game -> Game
looseIfNotWin game =
    if game.state == Win then
        game

    else
        { game | state = Loose }


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
