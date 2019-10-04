module View.FeatTable exposing (view)

import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Column
    exposing
        ( Column
        , columnToColumnLabel
        , columnToRecordToTextWithMaxes
        )
import Data.Cards as Cards
import Data.ColumnToggles as ColumnToggles
import Data.Sort as Sort
import Html exposing (Html, text)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import Html.Styled
import Html.Styled.Attributes as HSA
import Html.Styled.Keyed
import Library exposing (thrush)
import Renderer
import SavedFeat exposing (SavedFeat)
import Scores
    exposing
        ( Record
        )
import SortColumn exposing (SortColumn(..))
import View.FeatCards exposing (CardMsgs)


view :
    ColumnToggles.State
    -> Cards.State
    -> CardMsgs msg
    -> List SavedFeat
    -> Maybe (Html msg)
view tableState cardsState cardMsgs savedFeats =
    savedFeats
        |> List.sortWith
            (SavedFeat.compare
                cardsState.sort
            )
        |> savedFeatsToTable
            cardsState
            cardMsgs
            (ColumnToggles.columns tableState)


savedFeatsToTable :
    Cards.State
    -> CardMsgs msg
    -> List Column
    -> List SavedFeat
    -> Maybe (Html msg)
savedFeatsToTable cardsState cardMsgs cols savedFeats =
    savedFeats
        |> SavedFeat.maxRecord
        |> Maybe.map
            (savedFeatToRow
                cardMsgs
                cols
                >> List.map
                >> thrush savedFeats
                >> ([ [ ( "Index"
                        , Renderer.icon
                            (case ( cardsState.sort.sortColumn, cardsState.sort.sortOrder ) of
                                ( SortColumn.Index, Library.Ascending ) ->
                                    "sort-up"

                                ( SortColumn.Index, Library.Descending ) ->
                                    "sort-down"

                                ( _, _ ) ->
                                    "sort"
                            )
                            [ Sort.kindaFlip
                                cardsState.sort
                                SortColumn.Index
                                |> Cards.setSort cardsState
                                |> cardMsgs.cardsChanged
                                |> onClick
                            ]
                        )
                      ]
                    , [ ( "Note", text "" ) ]
                    , List.map
                        (\c -> ( columnToColumnLabel c, columnAndSortToIcon cardsState cardMsgs c ))
                        cols
                    , [ ( "Delete", text "" ) ]
                    ]
                        |> List.concat
                        |> Renderer.rowsToHeadedTable
                   )
            )


savedFeatToRow :
    CardMsgs msg
    -> List Column
    -> Record
    -> SavedFeat
    -> ( String, Html msg )
savedFeatToRow cardMsgs cols maxes savedFeat =
    [ [ .index >> String.fromInt >> text >> classToHtmlToStyledCell "body-cell--index"
      , savedFeatToNoteInput "table" cardMsgs
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
        , cardMsgs.deleteButtonClicked savedFeat.index |> Button.onClick
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


columnAndSortToIcon :
    Cards.State
    -> CardMsgs msg
    -> Column
    -> Html msg
columnAndSortToIcon cardsState cardMsgs column =
    case ( cardsState.sort.sortColumn, SortColumn.fromColumn column ) of
        ( s, Just sc ) ->
            (if s == sc then
                case cardsState.sort.sortOrder of
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
                    , Sort.kindaFlip cardsState.sort sc
                        |> Cards.setSort cardsState
                        |> cardMsgs.cardsChanged
                        |> onClick
                    ]

        ( _, Nothing ) ->
            text ""


savedFeatToNoteInput : String -> CardMsgs msg -> SavedFeat -> Html msg
savedFeatToNoteInput classSuffix cardMsgs savedFeat =
    Input.text
        [ Input.placeholder "Note"
        , Input.value <| .note <| savedFeat.feat
        , Input.onInput <| cardMsgs.noteChanged savedFeat.index
        , Input.attrs [ HA.class <| "note-input note-input--" ++ classSuffix ]
        ]
