module Counter exposing (main)

import Browser
import Debug exposing (toString)
import Html exposing (..)
import Html.Events exposing (onClick)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    Int


init : Model
init =
    0


type Msg
    = Inc
    | Reset


update : Msg -> Model -> Model
update msg model =
    case msg of
        Inc ->
            model + 1

        Reset ->
            0


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Reset ] [ text "Reset" ]
        , strong [] [ text (toString model) ]
        , button [ onClick Inc ] [ text "Inc" ]
        ]
