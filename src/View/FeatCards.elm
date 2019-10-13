module View.FeatCards exposing (CardMsgs, view)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Column exposing (Column)
import Data.Cards as Cards
import Data.ColumnToggles as ColumnToggles
import Data.Sort as Sort
import Dropdowns exposing (Option, typedSelect)
import Html exposing (Html, text)
import Html.Attributes as HA
import Html.Styled
import Library exposing (thrush)
import Renderer exposing (icon)
import SavedFeat exposing (IndexedSavedFeat, SavedFeat)
import SortColumn exposing (SortColumn(..))
import View.CurrentTable as CurrentTable


type alias NoteChangedMsg msg =
    Int -> String -> msg


type alias CardMsgs msg =
    { cardsChanged : Cards.State -> msg
    , noteChanged : Int -> String -> msg
    , deleteButtonClicked : Int -> msg
    , editButtonClicked : SavedFeat -> msg
    }


colDropdownFn : Cards.State -> CardMsgs msg -> ((Maybe SortColumn -> msg) -> a) -> a
colDropdownFn cardsState cardMsgs =
    Sort.setMaybeColumn cardsState.sort
        >> Cards.setSort cardsState
        >> cardMsgs.cardsChanged
        |> thrush


view : List IndexedSavedFeat -> ColumnToggles.State -> Cards.State -> CardMsgs msg -> List (Html msg)
view savedFeats tableState cardsState cardMsgs =
    [ Grid.row
        [ Row.attrs
            [ HA.style "margin-bottom" ".75rem"
            , HA.class "d-md-none"
            ]
        ]
        [ Grid.col [ Col.xs2 ] [ text "Sort by:" ]
        , tableState
            |> ColumnToggles.columns
            |> List.map SortColumn.fromColumn
            |> (::) (Just SortColumn.Index)
            |> List.filterMap
                (Maybe.map
                    (\sc ->
                        Option sc
                            (SortColumn.toString sc)
                            (SortColumn.toString sc)
                    )
                )
            |> typedSelect [ Select.small ]
            |> thrush cardsState.sort.sortColumn
            |> colDropdownFn cardsState cardMsgs
            |> List.singleton
            |> Grid.col [ Col.xs8 ]
        , icon
            (case cardsState.sort.sortOrder of
                Library.Ascending ->
                    "sort-amount-down-alt"

                Library.Descending ->
                    "sort-amount-up"
            )
            []
            |> List.singleton
            |> Button.button
                [ Button.outlineSecondary
                , cardsState.sort
                    |> Sort.toggleOrder
                    |> Cards.setSort cardsState
                    |> cardMsgs.cardsChanged
                    |> Button.onClick
                , Button.small
                , Button.block
                ]
            |> List.singleton
            |> Grid.col [ Col.xs2 ]
        ]
    , Grid.row []
        [ savedFeats
            |> List.sortWith (SavedFeat.compare cardsState.sort)
            |> cards
                (ColumnToggles.columns tableState)
                cardMsgs
            |> Card.keyedColumns
            |> List.singleton
            |> Grid.col [ Col.xs12, Col.attrs [ HA.class "d-md-none" ] ]
        ]
    ]


cards : List Column -> CardMsgs msg -> List IndexedSavedFeat -> List ( String, Card.Config msg )
cards cols cardMsgs indexedSavedFeats =
    indexedSavedFeats |> List.map (card cols (List.map .savedFeat indexedSavedFeats) cardMsgs)


card : List Column -> List SavedFeat -> CardMsgs msg -> IndexedSavedFeat -> ( String, Card.Config msg )
card cols feats cardMsgs indexedSavedFeat =
    ( indexedSavedFeat.savedFeat.key |> String.fromInt
    , Card.config
        [ Card.attrs []
        ]
        |> Card.headerH4 []
            [ text <| String.fromInt <| indexedSavedFeat.index
            , Button.button
                [ Button.outlineSecondary
                , cardMsgs.editButtonClicked indexedSavedFeat.savedFeat |> Button.onClick
                , Button.attrs
                    [ HA.class "card-edit"
                    , HA.title "Edit this"
                    ]
                ]
                [ icon "edit" []
                ]
            , Button.button
                [ Button.outlineDanger
                , cardMsgs.deleteButtonClicked indexedSavedFeat.savedFeat.key |> Button.onClick
                , Button.attrs
                    [ HA.class "card-delete"
                    , HA.title "Delete this"
                    ]
                ]
                [ icon "trash" []
                ]
            , savedFeatToNoteInput "card" indexedSavedFeat.savedFeat cardMsgs.noteChanged
            ]
        |> Card.block []
            [ Block.custom <| Html.Styled.toUnstyled <| CurrentTable.view feats cols indexedSavedFeat.savedFeat.feat ]
    )


savedFeatToNoteInput : String -> SavedFeat -> NoteChangedMsg msg -> Html msg
savedFeatToNoteInput classSuffix savedFeat noteChangedMsg =
    Input.text
        [ Input.placeholder "Note"
        , Input.value <| .note <| savedFeat.feat
        , Input.onInput <| noteChangedMsg savedFeat.key
        , Input.attrs [ HA.class <| "note-input note-input--" ++ classSuffix ]
        ]
