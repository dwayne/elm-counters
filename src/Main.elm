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


type alias Model = List Counter


type alias Counter = Int


init : Model
init = [0, 5, 10]


type Msg
    = Decrement Int
    | Increment Int
    | Reset Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        Decrement i ->
            List.indexedMap (\j counter -> if j == i then counter - 1 else counter) model

        Increment i ->
            List.indexedMap (\j counter -> if j == i then counter + 1 else counter) model

        Reset i ->
            List.indexedMap (\j counter -> if j == i then 0 else counter) model


view : Model -> Html Msg
view model =
    div [] (List.indexedMap viewCounter model)


viewCounter : Int -> Counter -> Html Msg
viewCounter i counter =
    div []
        [ button [ onClick (Decrement i) ] [ text "-" ]
        , text (String.fromInt counter)
        , button [ onClick (Increment i) ] [ text "+" ]
        , button [ onClick (Reset i) ] [ text "Reset" ]
        ]
