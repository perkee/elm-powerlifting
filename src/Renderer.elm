module Renderer exposing (FloatField, floatToString, initFloatField, maybeFloatToString, rowsToHeadedTable, stringToFloatField, stringToHeaderCell)

import Bootstrap.Table as Table
import Html as H exposing (Html)
import Html.Attributes as HA
import Library exposing (stringToAttr, truncate)



-- helper


stringToHeaderCell : ( String, Html msg ) -> Table.Cell msg
stringToHeaderCell ( title, icon ) =
    let
        class =
            title
                |> stringToAttr
                |> (++) "title-cell--"
    in
    Table.th [ Table.cellAttr (HA.class class) ]
        [ title |> H.text
        , icon
        ]


rowsToHeadedTable : List ( String, Html msg ) -> List ( String, Table.Row msg ) -> Html msg
rowsToHeadedTable titles rows =
    if List.isEmpty rows then
        H.span [] []

    else
        Table.table
            { options = [ Table.striped, Table.hover, Table.small ]
            , thead =
                Table.simpleThead
                    (titles |> List.map stringToHeaderCell)
            , tbody = Table.keyedTBody [] rows
            }


maybeFloatToString : Maybe Float -> String
maybeFloatToString f =
    case f of
        Just float ->
            floatToString float

        Nothing ->
            "—"


floatToString : Float -> String
floatToString =
    truncate 2 >> String.fromFloat



-- Float Fields


type alias FloatField =
    { value : Maybe Float
    , input : String
    }


initFloatField : FloatField
initFloatField =
    { value = Nothing
    , input = ""
    }


stringToFloatField : String -> FloatField
stringToFloatField str =
    case String.toFloat str of
        Nothing ->
            { value = Nothing, input = str }

        Just new ->
            { value = Just new, input = str }
