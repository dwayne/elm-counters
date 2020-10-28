module Main exposing (main)


import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


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


type Msg
    = Decrement
    | Increment


update : Msg -> Model -> Model
update msg model =
    case msg of
        Decrement ->
            model - 1

        Increment ->
            model + 1


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , text (String.fromInt model)
        , button [ onClick Increment ] [ text "+" ]
        ]
