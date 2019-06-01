module Library exposing
    ( filterListByList
    , removeAt
    , replace
    , stringToAttr
    , thrush
    , truncate
    , updateArrayAt
    , isStringPositiveFloat
    )

import Array exposing (Array)
import Regex


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


removeAt : Int -> Array any -> Array any
removeAt idx array =
    array
        |> Array.slice (idx + 1) (Array.length array)
        |> Array.append (array |> Array.slice 0 idx)

isStringPositiveFloat : String -> Bool
isStringPositiveFloat =
    Regex.fromString "^\\d*\\.?\\d*$"
        |> Maybe.withDefault Regex.never
        |> Regex.contains
