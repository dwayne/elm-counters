module Main exposing (main)


import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model = List Counter


type alias Counter = Int


init : () -> (Model, Cmd Msg)
init _ =
    ( [0, 5, 10]
    , Cmd.none
    )


type Msg
    = Decrement Int
    | Increment Int
    | Reset Int
    | Add
    | Remove Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Decrement i ->
            ( listUpdateAt i (\counter -> counter - 1) model
            , Cmd.none
            )

        Increment i ->
            ( listUpdateAt i (\counter -> counter + 1) model
            , Cmd.none
            )

        Reset i ->
            ( listUpdateAt i (always 0) model
            , Cmd.none
            )

        Add ->
            ( 0 :: model
            , Cmd.none
            )

        Remove i ->
            ( listIndexedFilter (\j _ -> j /= i) model
            , Cmd.none
            )


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
        , button [ onClick (Remove i) ] [ text "Remove" ]
        ]


-- HELPERS


listUpdateAt : Int -> (a -> a) -> List a -> List a
listUpdateAt i f =
    List.indexedMap (\j x -> if j == i then f x else x)


listIndexedFilter : (Int -> a -> Bool) -> List a -> List a
listIndexedFilter =
    listIndexedFilterHelper 0


listIndexedFilterHelper : Int -> (Int -> a -> Bool) -> List a -> List a
listIndexedFilterHelper i pred xs =
    case xs of
        [] ->
            []

        (x::rest) ->
            if pred i x then
                x :: listIndexedFilterHelper (i + 1) pred rest
            else
                listIndexedFilterHelper (i + 1) pred rest
