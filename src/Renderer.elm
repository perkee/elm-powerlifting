module Renderer exposing
    ( floatToString
    , icon
    , maybeFloatToString
    , rowsToHeadedTable
    , stringToHeaderCell
    )

-- import Html as H exposing (Html)

import Bootstrap.Table as Table
import Css
import Html as Raw
import Html.Attributes
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA
import Html.Styled.Keyed
import Library exposing (stringToAttr, thrush, truncate)



-- helper


stringToHeaderCell : ( String, Html msg ) -> ( String, Html msg )
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
    , H.th [ HA.class class ]
        [ title |> H.text
        , arrows
        ]
    )


rowsToHeadedTable : List ( String, Raw.Html msg ) -> List ( String, Raw.Html msg ) -> Raw.Html msg
rowsToHeadedTable titles rows =
    H.toUnstyled <|
        if List.isEmpty rows then
            H.text ""

        else
            H.table
                [ HA.class "table table-striped table-hover table-sm"
                ]
                [ titles
                    |> List.map
                        (Tuple.mapSecond H.fromUnstyled
                            >> stringToHeaderCell
                        )
                    |> Html.Styled.Keyed.node "tr" []
                    |> List.singleton
                    |> H.thead []
                , rows
                    |> List.map (Tuple.mapSecond H.fromUnstyled)
                    |> Html.Styled.Keyed.node "tbody" []
                ]


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


icon : String -> List (Raw.Attribute msg) -> Raw.Html msg
icon faClass attrs =
    faClass
        |> (++) "fa fa-"
        |> Html.Attributes.class
        |> (::)
        |> thrush attrs
        |> Raw.span
        |> thrush []
