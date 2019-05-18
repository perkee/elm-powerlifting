module Library exposing
    ( filterListByList
    , removeAt
    , replace
    , stringToAttr
    , thrush
    , truncate
    , updateArrayAt
    )

import Array exposing (Array)
import Regex
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


replace : String -> String -> String -> String
replace regex replacement =
    Regex.replace (Maybe.withDefault Regex.never <| Regex.fromString regex) (\_ -> replacement)


stringToAttr : String -> String
stringToAttr =
    replace "[)._]" ""
        >> replace "(\\s\\(?)" "-"
        >> String.toLower


removeAt : Array any -> Int -> Array any
removeAt array idx =
    array
        |> Array.slice (idx + 1) (Array.length array)
        |> Array.append (array |> Array.slice 0 idx)
