module Renderer exposing
    ( floatToString
    , icon
    , maybeFloatToString
    , rowsToHeadedTable
    , stringToHeaderCell
    , styledIcon
    )

-- import Html as H exposing (Html)

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


rowsToHeadedTable : List ( String, Html msg ) -> List ( String, Html msg ) -> Html msg
rowsToHeadedTable titles rows =
    if List.isEmpty rows then
        H.text ""

    else
        H.table
            [ HA.class "table table-striped table-hover table-sm"
            ]
            [ titles
                |> List.map
                    stringToHeaderCell
                |> Html.Styled.Keyed.node "tr" []
                |> List.singleton
                |> H.thead []
            , rows
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


styledIcon : String -> List (H.Attribute msg) -> Html msg
styledIcon faClass attrs =
    faClass
        |> (++) "fa fa-"
        |> HA.class
        |> (::)
        |> thrush attrs
        |> H.span
        |> thrush []
