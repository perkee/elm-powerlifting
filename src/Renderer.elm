module Renderer exposing (rowsToHeadedTable, textual)

import Html as H exposing (Html)



-- helper


textual : (List (H.Attribute msg) -> List (Html msg) -> Html msg) -> String -> Html msg
textual elem s =
    s |> H.text |> List.singleton |> elem []


rowsToHeadedTable : List String -> List (Html msg) -> Html msg
rowsToHeadedTable titles =
    H.tbody []
        >> List.singleton
        >> (titles
                |> List.map (textual H.th)
                |> H.tr []
                |> List.singleton
                |> H.thead []
                |> (::)
           )
        >> H.table []
