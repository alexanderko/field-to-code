module Game exposing (Action(..), Effect(..), EffectIcon(..), Game, GameState(..), buildNewGame, clearEffects, looseIfNotWin, updateGame)

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
    | FireIcon


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
        |> when (always (not (isTurn action))) doEnemyHit


isTurn : Action -> Bool
isTurn action =
    case action of
        Turn _ ->
            True

        _ ->
            False


doEnemyHit : Game -> Game
doEnemyHit game =
    { game | effects = game.effects ++ enemyHit }


enemyHit : List Effect
enemyHit =
    [ CellEffect FireIcon ( 1, 0 )
    , CellEffect FireIcon ( 1, 1 )
    ]


when : (Game -> Bool) -> (Game -> Game) -> Game -> Game
when guard updateFn game =
    if guard game then
        updateFn game

    else
        game


stateIsPending : Game -> Bool
stateIsPending { state } =
    state == Pending


deriveNewStateFromEffects : Game -> GameState
deriveNewStateFromEffects game =
    if effectsUnit .player FireIcon game then
        Loose

    else if effectsUnit .enemy HitIcon game then
        Win

    else
        Pending


effectsUnit : (Game -> Unit) -> EffectIcon -> Game -> Bool
effectsUnit getUnit icon game =
    let
        unit =
            getUnit game

        expectedEffect =
            CellEffect icon unit.coord
    in
    List.member expectedEffect game.effects


clearEffects : Game -> Game
clearEffects game =
    let
        state =
            deriveNewStateFromEffects game
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
