module Library exposing (filterListByList, thrush, truncate, updateArrayAt)

import Array exposing (Array)
import Set exposing (Set)


thrush : a -> (a -> b) -> b
thrush input fn =
    fn input


updateArrayAt : Int -> (a -> a) -> Array a -> Array a
updateArrayAt index fn array =
    case Array.get index array of
        Just value ->
            Array.set index (fn value) array

        Nothing ->
            array


setwiseToggle : comparable -> Set comparable -> Set comparable
setwiseToggle val set =
    if Set.member val set then
        Set.remove val set

    else
        Set.insert val set


truncate : Int -> Float -> Float
truncate places n =
    let
        factor =
            10.0 ^ toFloat places
    in
    n
        |> (*) factor
        |> round
        |> toFloat
        |> (\m -> m / factor)


filterListByList : List a -> List a -> List a
filterListByList filter list =
    List.filter (\element -> List.member element filter) list
