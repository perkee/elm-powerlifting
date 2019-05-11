module Renderer exposing (floatToString, htmlsToRow, maybeFloatToString, rowsToHeadedTable, textual)

import Bootstrap.Table as Table
import Html as H exposing (Html)
import Library exposing (truncate)



-- helper


textual : (List (H.Attribute msg) -> List (Html msg) -> Html msg) -> String -> Html msg
textual elem s =
    s |> H.text |> List.singleton |> elem []


htmlsToRow : List (Html msg) -> Table.Row msg
htmlsToRow =
    List.map (List.singleton >> Table.td [])
        >> Table.tr []


rowsToHeadedTable : List String -> List (Table.Row msg) -> Html msg
rowsToHeadedTable titles rows =
    if List.isEmpty rows then
        H.span [] []

    else
        Table.table
            { options = [ Table.striped, Table.hover ]
            , thead =
                Table.simpleThead
                    (titles |> List.map (H.text >> List.singleton >> Table.th []))
            , tbody = Table.tbody [] rows
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
