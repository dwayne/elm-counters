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
    | Add


update : Msg -> Model -> Model
update msg model =
    case msg of
        Decrement i ->
            listUpdateAt i (\counter -> counter - 1) model

        Increment i ->
            listUpdateAt i (\counter -> counter + 1) model

        Reset i ->
            listUpdateAt i (always 0) model

        Add ->
            0 :: model


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Add ] [ text "Add" ]
        , div [] (List.indexedMap viewCounter model)
        ]


viewCounter : Int -> Counter -> Html Msg
viewCounter i counter =
    div []
        [ button [ onClick (Decrement i) ] [ text "-" ]
        , text (String.fromInt counter)
        , button [ onClick (Increment i) ] [ text "+" ]
        , button [ onClick (Reset i) ] [ text "Reset" ]
        ]


-- HELPERS


listUpdateAt : Int -> (a -> a) -> List a -> List a
listUpdateAt i f =
    List.indexedMap (\j x -> if j == i then f x else x)
