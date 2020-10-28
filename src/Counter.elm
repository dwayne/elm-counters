module Counter exposing
    ( Counter

    -- CONSTRUCTORS
    , int, nonNegative, step

    -- MODIFIERS
    , decrement, increment, reset

    -- VIEW
    , view
    )


import Html exposing (Html, button, div, text)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)


type Counter
    = Integer Int
    | NonNegative Int
    | Step Int Int


int : Int -> Counter
int n =
    Integer n


nonNegative : Int -> Counter
nonNegative n =
    NonNegative (max 0 n)


step : Int -> Int -> Counter
step by n =
    Step (max 2 by) n


decrement : Counter -> Counter
decrement counter =
    case counter of
        Integer n ->
            Integer (n - 1)

        NonNegative n ->
            NonNegative (max 0 (n - 1))

        Step by n ->
            Step by (n - by)


increment : Counter -> Counter
increment counter =
    case counter of
        Integer n ->
            Integer (n + 1)

        NonNegative n ->
            NonNegative (n + 1)

        Step by n ->
            Step by (n + by)


reset : Counter -> Counter
reset counter =
    case counter of
        Integer _ ->
            Integer 0

        NonNegative _ ->
            NonNegative 0

        Step by _ ->
            Step by 0


view : msg -> msg -> msg -> msg -> Counter -> Html msg
view decrementMsg incrementMsg resetMsg removeMsg counter =
    case counter of
        Integer n ->
            div []
                [ button [ onClick decrementMsg ] [ text "-" ]
                , text (String.fromInt n)
                , button [ onClick incrementMsg ] [ text "+" ]
                , button [ onClick resetMsg ] [ text "Reset" ]
                , button [ onClick removeMsg ] [ text "Remove" ]
                ]

        NonNegative n ->
            div []
                [ button [ onClick decrementMsg, disabled (n == 0) ] [ text "-" ]
                , text (String.fromInt n)
                , button [ onClick incrementMsg ] [ text "+" ]
                , button [ onClick resetMsg ] [ text "Reset" ]
                , button [ onClick removeMsg ] [ text "Remove" ]
                ]

        Step by n ->
            div []
                [ button [ onClick decrementMsg ] [ text ("-" ++ String.fromInt by) ]
                , text (String.fromInt n)
                , button [ onClick incrementMsg ] [ text ("+" ++ String.fromInt by) ]
                , button [ onClick resetMsg ] [ text "Reset" ]
                , button [ onClick removeMsg ] [ text "Remove" ]
                ]
