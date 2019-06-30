module Renderer exposing
    ( floatToString
    , icon
    , maybeFloatToString
    , rowsToHeadedTable
    , stringToHeaderCell
    )

import Bootstrap.Table as Table
import Html as H exposing (Html)
import Html.Attributes as HA
import Library exposing (stringToAttr, thrush, truncate)



-- helper


stringToHeaderCell : ( String, Html msg ) -> ( String, Table.Cell msg )
stringToHeaderCell ( title, arrows ) =
    let
        class =
            title
                |> stringToAttr
                |> (++) "title-cell--"
                |> (++)
                    (if arrows /= H.text "" then
                        "title-cell--sortable "

                     else
                        ""
                    )
    in
    ( class
    , Table.th [ Table.cellAttr (HA.class class) ]
        [ title |> H.text
        , arrows
        ]
    )


rowsToHeadedTable : List ( String, Html msg ) -> List ( String, Table.Row msg ) -> Html msg
rowsToHeadedTable titles rows =
    if List.isEmpty rows then
        H.span [] []

    else
        Table.table
            { options = [ Table.striped, Table.hover, Table.small ]
            , thead =
                titles
                    |> List.map stringToHeaderCell
                    |> Table.keyedTr []
                    |> List.singleton
                    |> Table.thead []
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


icon : String -> List (H.Attribute msg) -> H.Html msg
icon faClass attrs =
    faClass
        |> (++) "fa fa-"
        |> HA.class
        |> (::)
        |> thrush attrs
        |> H.span
        |> thrush []
