module Main exposing (main)


import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random

import Counter
import List.Extra


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model = List Counter.Counter


init : () -> (Model, Cmd Msg)
init _ =
    ( [ Counter.int 0
      , Counter.nonNegative 5
      , Counter.step 10 100
      ]
    , Cmd.none
    )


type Msg
    = Decrement Int
    | Increment Int
    | Reset Int
    | Add Int
    | AddRandom
    | Remove Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Decrement i ->
            ( List.Extra.updateAt i Counter.decrement model
            , Cmd.none
            )

        Increment i ->
            ( List.Extra.updateAt i Counter.increment model
            , Cmd.none
            )

        Reset i ->
            ( List.Extra.updateAt i Counter.reset model
            , Cmd.none
            )

        Add n ->
            ( Counter.int n :: model
            , Cmd.none
            )

        AddRandom ->
            ( model
            , Random.generate Add (Random.int -10 10)
            )

        Remove i ->
            ( List.Extra.indexedFilter (\j _ -> j /= i) model
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick (Add 0) ] [ text "Add" ]
        , button [ onClick AddRandom ] [ text "Add Random" ]
        , div [] <|
            List.indexedMap
                (\i -> Counter.view (Decrement i) (Increment i) (Reset i) (Remove i))
                model
        ]
