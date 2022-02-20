module Level exposing (..)

import Array exposing (Array)
import Coord exposing (Coord)
import Direction exposing (Direction(..))
import Game
    exposing
        ( Effect(..)
        , EffectIcon(..)
        , EnemyAction(..)
        , Game
        , GameSetup
        , buildNewGame
        )
import Unit exposing (Unit)


type alias LevelName =
    String


type Level
    = BasicLevel GameSetup LevelName


getName : Level -> LevelName
getName (BasicLevel _ name) =
    name


{-| Creates fire cell effect for passed coord list
-}
fireCells : List Coord -> List Effect
fireCells cells =
    List.map (CellEffect FireIcon) cells


newGame : Level -> Game
newGame (BasicLevel gameSetup _) =
    buildNewGame gameSetup


level_01 : Level
level_01 =
    BasicLevel
        { enemy = Unit ( 1, 2 ) Up
        , enemyAction = HitStatic (fireCells [ ( 1, 0 ), ( 1, 1 ) ])
        , player = Unit ( 1, 0 ) Up
        }
        "Dragon Fire Col"


level_02 : Level
level_02 =
    BasicLevel
        { enemy = Unit ( 1, 2 ) Up
        , enemyAction = HitStatic (fireCells [ ( 1, 1 ), ( 2, 1 ) ])
        , player = Unit ( 1, 0 ) Up
        }
        "Dragon Fire Row"


levels : Array Level
levels =
    Array.fromList
        [ level_01
        , level_02
        ]
