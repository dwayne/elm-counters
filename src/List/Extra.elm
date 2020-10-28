module List.Extra exposing (updateAt, indexedFilter)


updateAt : Int -> (a -> a) -> List a -> List a
updateAt i f =
    List.indexedMap (\j x -> if j == i then f x else x)


indexedFilter : (Int -> a -> Bool) -> List a -> List a
indexedFilter =
    indexedFilterHelper 0


indexedFilterHelper : Int -> (Int -> a -> Bool) -> List a -> List a
indexedFilterHelper i pred xs =
    case xs of
        [] ->
            []

        (x::rest) ->
            if pred i x then
                x :: indexedFilterHelper (i + 1) pred rest
            else
                indexedFilterHelper (i + 1) pred rest
