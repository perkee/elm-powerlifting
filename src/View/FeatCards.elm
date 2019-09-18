module View.FeatCards exposing (CardSorting, view)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Column
    exposing
        ( Column
        , columnToRecordToText
        , columnToRecordToTextWithMaxes
        , columnToToggleLabel
        )
import Data.ColumnToggles as ColumnToggles
import Dropdowns exposing (Option, typedSelect)
import Feat exposing (Feat)
import Html exposing (Html, h3, text)
import Html.Attributes exposing (class, style)
import Html.Styled
import Html.Styled.Attributes as HSA
import Library exposing (dropNothing, thrush)
import Renderer exposing (icon, rowsToHeadedTable)
import SavedFeat exposing (SavedFeat)
import Scores
    exposing
        ( featToRecord
        , maxRecord
        )
import SortColumn exposing (SortColumn(..))


type alias NoteChangedMsg msg =
    Int -> String -> msg


type alias CardSorting msg =
    { sortColumn : SortColumn.SortColumn
    , sortOrder : Library.SortOrder
    , sortColumnDropdownChanged : Maybe SortColumn.SortColumn -> msg
    , noteChanged : Int -> String -> msg
    , sortOrderToggleClicked : msg
    , deleteButtonClicked : Int -> msg
    }


view : List SavedFeat -> ColumnToggles.State -> CardSorting msg -> List (Html msg)
view savedFeats tableState cardSorting =
    [ h3 [ class "d-md-none" ] [ text "Grouped by Lifter" ]
    , Grid.row
        [ Row.attrs
            [ style "margin-bottom" ".75rem"
            , class "d-md-none"
            ]
        ]
        [ Grid.col [ Col.xs2 ] [ text "Sort by:" ]
        , tableState
            |> ColumnToggles.columns
            |> List.map SortColumn.fromColumn
            |> (::) (Just SortColumn.Index)
            |> dropNothing
            |> List.map
                (\sc ->
                    Option sc
                        (SortColumn.toString sc)
                        (SortColumn.toString sc)
                )
            |> typedSelect [ Select.small ]
            |> thrush cardSorting.sortColumn
            |> thrush cardSorting.sortColumnDropdownChanged
            |> List.singleton
            |> Grid.col [ Col.xs8 ]
        , icon
            (case cardSorting.sortOrder of
                Library.Ascending ->
                    "sort-amount-down-alt"

                Library.Descending ->
                    "sort-amount-up"
            )
            []
            |> List.singleton
            |> Button.button
                [ Button.outlineSecondary
                , Button.onClick cardSorting.sortOrderToggleClicked
                , Button.small
                , Button.block
                ]
            |> List.singleton
            |> Grid.col [ Col.xs2 ]
        ]
    , Grid.row []
        [ savedFeats
            |> List.sortWith (SortColumn.compareSavedFeats cardSorting.sortOrder cardSorting.sortColumn)
            |> savedFeatsToCards
                (ColumnToggles.columns tableState)
                cardSorting
            |> Card.keyedColumns
            |> List.singleton
            |> Grid.col [ Col.xs12, Col.attrs [ class "d-md-none" ] ]
        ]
    ]


savedFeatsToCards : List Column -> CardSorting msg -> List SavedFeat -> List ( String, Card.Config msg )
savedFeatsToCards cols cardSorting savedFeats =
    savedFeats |> List.map (savedFeatToCard cols savedFeats cardSorting)


savedFeatToCard : List Column -> List SavedFeat -> CardSorting msg -> SavedFeat -> ( String, Card.Config msg )
savedFeatToCard cols feats cardSorting savedFeat =
    ( savedFeat.key |> String.fromInt
    , Card.config
        [ Card.attrs []
        ]
        |> Card.headerH4 []
            [ text <| String.fromInt <| savedFeat.index
            , Button.button
                [ Button.outlineDanger
                , cardSorting.deleteButtonClicked savedFeat.index |> Button.onClick
                , Button.attrs [ class "card-delete" ]
                ]
                [ icon "trash" []
                ]
            , savedFeatToNoteInput "card" savedFeat cardSorting.noteChanged
            ]
        |> Card.block []
            [ Block.custom <| featToTable feats cols savedFeat.feat ]
    )


featToTable : List SavedFeat -> List Column -> Feat -> Html msg
featToTable savedFeats cols =
    let
        recordsToText =
            cols
                |> List.map
                    (if List.isEmpty savedFeats then
                        \c r -> columnToRecordToText c r |> Html.Styled.fromUnstyled

                     else
                        savedFeats
                            |> List.map (.feat >> featToRecord)
                            |> maxRecord
                            |> (\m c r -> columnToRecordToTextWithMaxes m c r |> Html.Styled.fromUnstyled)
                    )
    in
    featToRecord
        >> thrush
        >> List.map
        >> thrush recordsToText
        >> List.map2
            (\label value ->
                ( label |> columnToToggleLabel
                , Html.Styled.tr
                    []
                    [ label |> columnToToggleLabel |> Html.Styled.text |> List.singleton |> Html.Styled.td [ "body-cell--label" |> HSA.class ]
                    , value |> List.singleton |> Html.Styled.td [ "body-cell--value" |> HSA.class ]
                    ]
                )
            )
            cols
        >> List.map (Tuple.mapSecond Html.Styled.toUnstyled)
        >> rowsToHeadedTable [ ( "Label", text "" ), ( "Value", text "" ) ]


savedFeatToNoteInput : String -> SavedFeat -> NoteChangedMsg msg -> Html msg
savedFeatToNoteInput classSuffix savedFeat noteChangedMsg =
    Input.text
        [ Input.placeholder "Note"
        , Input.value <| .note <| savedFeat.feat
        , Input.onInput <| noteChangedMsg savedFeat.index
        , Input.attrs [ class <| "note-input note-input--" ++ classSuffix ]
        ]
