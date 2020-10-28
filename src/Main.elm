module Main exposing (main)


import Browser
import Html exposing (Html, button, div, text)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model = Int


init : Model
init = 0


type alias Msg = Never


update : Msg -> Model -> Model
update msg model = model


view : Model -> Html Msg
view model =
    div []
        [ button [] [ text "-" ]
        , text (String.fromInt model)
        , button [] [ text "+" ]
        ]
