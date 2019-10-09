module Library exposing
    ( SortOrder(..)
    , compareByOrder
    , compose2
    , compose2same
    , filterListByList
    , isStringPositiveFloat
    , maybeListHead
    , maybeListMember
    , maybeSnocnu
    , pam
    , phi
    , removeAt
    , replace
    , snocnu
    , stringToAttr
    , thrush
    , truncate
    , updateArrayAt
    , updateListAt
    )

import Array exposing (Array)
import Regex


thrush : a -> (a -> b) -> b
thrush input fn =
    fn input


pam : a -> List (a -> b) -> List b
pam arg =
    List.map (thrush arg)


updateListAt : Int -> (a -> a) -> List a -> List a
updateListAt index updater =
    List.indexedMap
        (\i a ->
            if i == index then
                updater a

            else
                a
        )


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


maybeListMember : List a -> Maybe a -> List a -> List a
maybeListMember beginning maybeMiddle end =
    case maybeMiddle of
        Just middle ->
            beginning ++ (middle :: end)

        Nothing ->
            beginning ++ end


maybeSnocnu : Maybe a -> List a -> List a
maybeSnocnu maybeEnd beginning =
    case maybeEnd of
        Just end ->
            snocnu end beginning

        Nothing ->
            beginning


snocnu : a -> List a -> List a
snocnu x xs =
    xs ++ [ x ]


maybeListHead : Maybe a -> List a -> List a
maybeListHead maybeBeginning end =
    case maybeBeginning of
        Just beginning ->
            beginning :: end

        Nothing ->
            end


phi : Float
phi =
    (1 + sqrt 5) / 2


type SortOrder
    = Ascending
    | Descending


compareByOrder : SortOrder -> comparable -> comparable -> Order
compareByOrder sortOrder a b =
    case ( sortOrder, compare a b ) of
        ( Ascending, anyOrder ) ->
            anyOrder

        ( Descending, LT ) ->
            GT

        ( Descending, GT ) ->
            LT

        ( Descending, EQ ) ->
            EQ


compose2 : (a -> b) -> (m -> n) -> (b -> n -> x) -> (a -> m -> x)
compose2 aToB mToN binaryFn a m =
    binaryFn (a |> aToB) (m |> mToN)


compose2same : (a -> b) -> (b -> b -> x) -> (a -> a -> x)
compose2same aToB binaryFn =
    compose2 aToB aToB binaryFn
