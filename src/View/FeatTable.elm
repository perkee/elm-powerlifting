module View.FeatTable exposing (view)

import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Column
    exposing
        ( Column
        , columnToColumnLabel
        , columnToRecordToTextWithMaxes
        )
import Data.ColumnToggles as ColumnToggles
import Html exposing (Html, h3, text)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import Html.Styled
import Html.Styled.Attributes as HSA
import Html.Styled.Keyed
import Library exposing (dropNothing, thrush)
import Renderer
import SavedFeat exposing (SavedFeat)
import Scores
    exposing
        ( Record
        )
import SortColumn exposing (SortColumn(..))
import View.FeatCards exposing (CardSorting)


view : ColumnToggles.State -> CardSorting msg -> List SavedFeat -> Html msg
view tableState cardSorting savedFeats =
    savedFeats
        |> List.sortWith
            (SortColumn.compareSavedFeats
                cardSorting.sortOrder
                cardSorting.sortColumn
            )
        |> savedFeatsToTable
            cardSorting
            (ColumnToggles.columns tableState)


savedFeatsToTable : CardSorting msg -> List Column -> List SavedFeat -> Html msg
savedFeatsToTable cardSorting cols savedFeats =
    savedFeats
        |> List.map (savedFeatToRow cardSorting cols <| SavedFeat.maxRecord savedFeats)
        >> ([ [ ( "Index"
                , Renderer.icon
                    (case ( cardSorting.sortColumn, cardSorting.sortOrder ) of
                        ( SortColumn.Index, Library.Ascending ) ->
                            "sort-up"

                        ( SortColumn.Index, Library.Descending ) ->
                            "sort-down"

                        ( _, _ ) ->
                            "sort"
                    )
                    [ onClick <| cardSorting.sortColumnArrowsClicked SortColumn.Index
                    ]
                )
              ]
            , [ ( "Note", text "" ) ]
            , List.map
                (\c -> ( columnToColumnLabel c, columnAndSortToIcon cardSorting c ))
                cols
            , [ ( "Delete", text "" ) ]
            ]
                |> List.concat
                |> Renderer.rowsToHeadedTable
           )


savedFeatToRow : CardSorting msg -> List Column -> Record -> SavedFeat -> ( String, Html msg )
savedFeatToRow cardSorting cols maxes savedFeat =
    [ [ .index >> String.fromInt >> text >> classToHtmlToStyledCell "body-cell--index"
      , savedFeatToNoteInput "table" cardSorting
            >> classToHtmlToStyledCell "body-cell--note"
      ]
        |> List.map (thrush savedFeat)
    , (savedFeat.feat
        |> Scores.featToRecord
        |> thrush
        |> List.map
      )
        (List.map (columnToFloatToStyledCell maxes) cols)
    , Button.button
        [ Button.outlineDanger
        , cardSorting.deleteButtonClicked savedFeat.index |> Button.onClick
        ]
        [ Renderer.icon "trash" []
        ]
        |> classToHtmlToStyledCell "body-cell--delete"
        |> List.singleton
    ]
        |> List.concat
        |> Html.Styled.Keyed.node "tr" []
        |> (\row -> ( savedFeat.key |> String.fromInt, row |> Html.Styled.toUnstyled ))


classToHtmlToStyledCell : String -> Html msg -> ( String, Html.Styled.Html msg )
classToHtmlToStyledCell className html =
    ( className, Html.Styled.td [ className |> Library.stringToAttr |> HSA.class ] [ Html.Styled.fromUnstyled html ] )


columnToFloatToStyledCell : Record -> Column -> Record -> ( String, Html.Styled.Html msg )
columnToFloatToStyledCell maxes col =
    columnToRecordToTextWithMaxes maxes col >> classToHtmlToStyledCell (col |> columnToColumnLabel |> (++) "body-cell--")


columnAndSortToIcon : CardSorting msg -> Column -> Html msg
columnAndSortToIcon cardSorting column =
    case ( cardSorting.sortColumn, SortColumn.fromColumn column ) of
        ( s, Just sc ) ->
            (if s == sc then
                case cardSorting.sortOrder of
                    Library.Ascending ->
                        "sort-up"

                    Library.Descending ->
                        "sort-down"

             else
                "sort"
            )
                |> Renderer.icon
                |> thrush
                    [ HA.class "sort-button"
                    , onClick <| cardSorting.sortColumnArrowsClicked sc
                    ]

        ( _, Nothing ) ->
            text ""


savedFeatToNoteInput : String -> CardSorting msg -> SavedFeat -> Html msg
savedFeatToNoteInput classSuffix cardSorting savedFeat =
    Input.text
        [ Input.placeholder "Note"
        , Input.value <| .note <| savedFeat.feat
        , Input.onInput <| cardSorting.noteChanged savedFeat.index
        , Input.attrs [ HA.class <| "note-input note-input--" ++ classSuffix ]
        ]
