module Renderer exposing (floatToString, htmlsToRow, maybeFloatToString, rowsToHeadedTable, textual)

import Html as H exposing (Html)
import Library exposing (truncate)



-- helper


textual : (List (H.Attribute msg) -> List (Html msg) -> Html msg) -> String -> Html msg
textual elem s =
    s |> H.text |> List.singleton |> elem []


htmlsToRow : List (Html msg) -> Html msg
htmlsToRow =
    List.map (List.singleton >> H.td [])
        >> H.tr []


rowsToHeadedTable : List String -> List (Html msg) -> Html msg
rowsToHeadedTable titles rows =
    if List.isEmpty rows then
        H.span [] []

    else
        rows
            |> H.tbody []
            >> List.singleton
            >> (titles
                    |> List.map (textual H.th)
                    |> H.tr []
                    |> List.singleton
                    |> H.thead []
                    |> (::)
               )
            >> H.table []


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
