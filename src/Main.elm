module Main exposing (main)


import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)
import Random


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model = List Counter


init : () -> (Model, Cmd Msg)
init _ =
    ( [ counterInt 0
      , counterNonNegative 5
      , counterStep 10 100
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
            ( listUpdateAt i counterDecrement model
            , Cmd.none
            )

        Increment i ->
            ( listUpdateAt i counterIncrement model
            , Cmd.none
            )

        Reset i ->
            ( listUpdateAt i counterReset model
            , Cmd.none
            )

        Add n ->
            ( counterInt n :: model
            , Cmd.none
            )

        AddRandom ->
            ( model
            , Random.generate Add (Random.int -10 10)
            )

        Remove i ->
            ( listIndexedFilter (\j _ -> j /= i) model
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick (Add 0) ] [ text "Add" ]
        , button [ onClick AddRandom ] [ text "Add Random" ]
        , div [] <|
            List.indexedMap
                (\i counter -> counterView (Decrement i) (Increment i) (Reset i) (Remove i) counter)
                model
        ]


-- COUNTER


type Counter
    = Integer Int
    | NonNegative Int
    | Step Int Int


counterInt : Int -> Counter
counterInt n =
    Integer n


counterNonNegative : Int -> Counter
counterNonNegative n =
    NonNegative (max 0 n)


counterStep : Int -> Int -> Counter
counterStep by n =
    Step (max 2 by) n


counterDecrement : Counter -> Counter
counterDecrement counter =
    case counter of
        Integer n ->
            Integer (n - 1)

        NonNegative n ->
            NonNegative (max 0 (n - 1))

        Step by n ->
            Step by (n - by)


counterIncrement : Counter -> Counter
counterIncrement counter =
    case counter of
        Integer n ->
            Integer (n + 1)

        NonNegative n ->
            NonNegative (n + 1)

        Step by n ->
            Step by (n + by)


counterReset : Counter -> Counter
counterReset counter =
    case counter of
        Integer _ ->
            Integer 0

        NonNegative _ ->
            NonNegative 0

        Step by _ ->
            Step by 0


counterView : msg -> msg -> msg -> msg -> Counter -> Html msg
counterView decrement increment reset remove counter =
    case counter of
        Integer n ->
            div []
                [ button [ onClick decrement ] [ text "-" ]
                , text (String.fromInt n)
                , button [ onClick increment ] [ text "+" ]
                , button [ onClick reset ] [ text "Reset" ]
                , button [ onClick remove ] [ text "Remove" ]
                ]

        NonNegative n ->
            div []
                [ button [ onClick decrement, disabled (n == 0) ] [ text "-" ]
                , text (String.fromInt n)
                , button [ onClick increment ] [ text "+" ]
                , button [ onClick reset ] [ text "Reset" ]
                , button [ onClick remove ] [ text "Remove" ]
                ]

        Step by n ->
            div []
                [ button [ onClick decrement ] [ text ("-" ++ String.fromInt by) ]
                , text (String.fromInt n)
                , button [ onClick increment ] [ text ("+" ++ String.fromInt by) ]
                , button [ onClick reset ] [ text "Reset" ]
                , button [ onClick remove ] [ text "Remove" ]
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
