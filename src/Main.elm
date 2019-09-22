module Main exposing (main)

-- (Html, button, div, text, input, option, select)
-- imports used

import Array exposing (Array)
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import Browser
import Column
    exposing
        ( Column
        , columnToRecordToText
        , columnToRecordToTextWithMaxes
        , columnToToggleLabel
        , initCurrentColumns
        , initTableColumns
        )
import Data.Cards as Cards
import Data.ColumnToggles as ColumnToggles
import Feat exposing (Feat, MassUnit, testFeats)
import Html exposing (Html, div, h1, h2, h3, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Html.Styled
import Html.Styled.Attributes as HSA
import Library exposing (SortOrder(..), removeAt, thrush, updateArrayAt)
import LiftForm
import Renderer exposing (rowsToHeadedTable)
import SavedFeat exposing (SavedFeat)
import Scores
    exposing
        ( featToRecord
        , maxRecord
        )
import SortColumn
import View.Cards as Cards
import View.ColumnToggles as ColumnToggles
import View.FeatCards as FeatCards
import View.FeatTable as FeatTable


main : Platform.Program String Model Msg
main =
    -- Flags is only one field, so type is String.
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ LiftForm.subscriptions model.formState FormUpdated
        , ColumnToggles.subscriptions model.featState FeatDisplayUpdated
        , ColumnToggles.subscriptions model.tableState TableDisplayUpdated
        , Modal.subscriptions model.deleteConfirmVisibility DeleteModalAnimated
        ]


type alias Model =
    { formState : LiftForm.State
    , feats : Array SavedFeat
    , featKey : Int
    , featState : ColumnToggles.State
    , tableState : ColumnToggles.State
    , deleteConfirmVisibility : Modal.Visibility
    , idxToDelete : Maybe Int
    , sortColumn : SortColumn.SortColumn
    , sortOrder : Library.SortOrder
    , liftCardUnits : MassUnit
    , cardsState : Cards.State
    }


someFeats : Array SavedFeat
someFeats =
    testFeats
        |> List.map2 (\( index, key ) feat -> SavedFeat feat index key)
            [ ( 0, 2 )
            , ( 1, 2 )
            , ( 2, 2 )
            ]
        |> Array.fromList


init : String -> ( Model, Cmd Msg )
init nodeEnv =
    ( { formState = LiftForm.init
      , feats =
            if nodeEnv == "development" then
                --Array.empty
                someFeats

            else
                Array.empty
      , featKey = 0
      , featState = ColumnToggles.init initCurrentColumns
      , tableState = ColumnToggles.init initTableColumns
      , deleteConfirmVisibility = Modal.hidden
      , idxToDelete = Nothing
      , sortOrder = Library.Ascending
      , sortColumn = SortColumn.Index
      , liftCardUnits = Feat.KG
      , cardsState = Cards.init
      }
    , Cmd.none
    )


modelToFeat : Model -> Maybe Feat
modelToFeat =
    .formState >> LiftForm.toFeat


type Msg
    = FormUpdated LiftForm.State
    | SaveButtonClicked (Maybe Feat)
    | NoteChanged Int String
    | DeleteModalAnimated Modal.Visibility
    | FeatDisplayUpdated ColumnToggles.State
    | TableDisplayUpdated ColumnToggles.State
    | DeleteButtonClicked Int
    | DeleteCanceled
    | DeleteConfirmed
    | CardsUpdate Cards.State
    | ColumnHeaderArrowsClicked SortColumn.SortColumn
    | SortColumnDropdownChanged (Maybe SortColumn.SortColumn)
    | SortOrderToggleClicked
    | LiftCardUnitsToggleClicked


setNoteOnFeat : String -> Feat -> Feat
setNoteOnFeat note feat =
    { feat | note = note }


setNoteOnSavedFeat : String -> SavedFeat -> SavedFeat
setNoteOnSavedFeat note savedFeat =
    { savedFeat | feat = setNoteOnFeat note savedFeat.feat }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        FeatDisplayUpdated state ->
            { model | featState = state }

        TableDisplayUpdated state ->
            { model | tableState = state }

        FormUpdated state ->
            { model | formState = state }

        SaveButtonClicked mf ->
            case mf of
                Just feat ->
                    { model
                        | feats = Array.push (SavedFeat feat (Array.length model.feats) model.featKey) model.feats
                        , featKey = model.featKey + 1
                    }

                Nothing ->
                    model

        NoteChanged index note ->
            { model | feats = updateArrayAt index (setNoteOnSavedFeat note) model.feats }

        DeleteCanceled ->
            { model
                | deleteConfirmVisibility = Modal.hidden
                , idxToDelete = Nothing
            }

        DeleteConfirmed ->
            { model
                | deleteConfirmVisibility = Modal.hidden
                , feats =
                    case model.idxToDelete of
                        Just idx ->
                            model.feats
                                |> removeAt idx
                                |> Array.indexedMap
                                    (\i f ->
                                        if i < idx then
                                            f

                                        else
                                            { f | index = f.index - 1 }
                                    )

                        Nothing ->
                            model.feats
                , idxToDelete = Nothing
            }

        DeleteButtonClicked idx ->
            { model
                | deleteConfirmVisibility = Modal.shown
                , idxToDelete = Just idx
            }

        DeleteModalAnimated visibility ->
            -- Just fadein
            { model | deleteConfirmVisibility = visibility }

        ColumnHeaderArrowsClicked sortColumn ->
            { model
                | sortColumn = sortColumn
                , sortOrder =
                    if model.sortColumn /= sortColumn then
                        Descending

                    else
                        case model.sortOrder of
                            Ascending ->
                                Descending

                            Descending ->
                                Ascending
            }

        SortColumnDropdownChanged maybeSortColumn ->
            case maybeSortColumn of
                Just sortColumn ->
                    { model
                        | sortColumn = sortColumn
                    }

                Nothing ->
                    model

        SortOrderToggleClicked ->
            { model
                | sortOrder =
                    case model.sortOrder of
                        Ascending ->
                            Descending

                        Descending ->
                            Ascending
            }

        LiftCardUnitsToggleClicked ->
            { model
                | liftCardUnits =
                    case model.liftCardUnits of
                        Feat.KG ->
                            Feat.LBM

                        Feat.LBM ->
                            Feat.KG
            }

        CardsUpdate state ->
            { model | cardsState = state }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    div []
        [ Grid.container []
            ([ h1 [] [ text "Every Score Calculator" ]
             , LiftForm.view model.formState FormUpdated <| SaveButtonClicked <| modelToFeat <| model
             , h2 [] [ text "Current Score" ]
             , Grid.row [ Row.attrs [ class "current-table" ] ]
                [ Grid.col [ Col.xs12 ]
                    (case modelToFeat model of
                        Just feat ->
                            [ ColumnToggles.config FeatDisplayUpdated "current-column-toggles"
                                |> ColumnToggles.title "Current Scores Options"
                                |> ColumnToggles.view model.featState
                            , (featToTable model.feats <| ColumnToggles.columns model.featState) <| feat
                            ]

                        Nothing ->
                            [ Alert.simpleInfo [] [ text "Enter data to see all scores for a lift" ] ]
                    )
                ]
             , h2 [] [ text "Scores Table" ]
             , if Array.isEmpty model.feats then
                Alert.simpleInfo [] [ text "Add scores to the table to compare" ]

               else
                ColumnToggles.config TableDisplayUpdated "table-column-toggles"
                    |> ColumnToggles.title "Table Options"
                    |> ColumnToggles.view model.tableState
                    |> List.singleton
                    |> Grid.col [ Col.xs12 ]
                    |> List.singleton
                    |> Grid.row [ Row.attrs [ style "margin-bottom" ".75rem" ] ]
             ]
                ++ Cards.view (Array.toList model.feats)
                    model.tableState
                    (FeatCards.CardSorting model.sortColumn
                        model.sortOrder
                        SortColumnDropdownChanged
                        ColumnHeaderArrowsClicked
                        NoteChanged
                        SortOrderToggleClicked
                        DeleteButtonClicked
                        model.liftCardUnits
                        LiftCardUnitsToggleClicked
                    )
            )
        , Grid.containerFluid [ class "d-none d-md-block" ]
            [ Grid.row []
                [ Grid.col [ Col.sm12 ]
                    [ model.feats
                        |> Array.toList
                        |> FeatTable.view
                            model.tableState
                            (FeatCards.CardSorting model.sortColumn
                                model.sortOrder
                                SortColumnDropdownChanged
                                ColumnHeaderArrowsClicked
                                NoteChanged
                                SortOrderToggleClicked
                                DeleteButtonClicked
                                model.liftCardUnits
                                LiftCardUnitsToggleClicked
                            )
                    ]
                ]
            ]
        , Modal.config DeleteCanceled
            |> Modal.withAnimation DeleteModalAnimated
            |> Modal.large
            |> Modal.h3 [] [ text "Confirm delete" ]
            |> Modal.body []
                [ Grid.containerFluid []
                    [ Grid.row []
                        [ Grid.col
                            [ Col.xs12 ]
                            [ (case model.idxToDelete of
                                Just idx ->
                                    "Are you sure you want to delete the entry at row "
                                        ++ String.fromInt idx

                                Nothing ->
                                    ""
                              )
                                |> text
                            ]
                        ]
                    ]
                ]
            |> Modal.footer []
                [ Button.button
                    [ Button.outlineDanger

                    -- cannot currently animate this on confirm. Gets into an infinite loop rn. Kinda impresseive!
                    , Button.attrs [ onClick <| DeleteConfirmed ]
                    ]
                    [ text "Yes, Delete this row" ]
                , Button.button
                    [ Button.outlineSuccess
                    , Button.attrs [ onClick <| DeleteCanceled ]
                    ]
                    [ text "Nevermind" ]
                ]
            |> Modal.view model.deleteConfirmVisibility
        ]


featToTable : Array SavedFeat -> List Column -> Feat -> Html Msg
featToTable savedFeats cols =
    let
        recordsToText =
            cols
                |> List.map
                    (if Array.isEmpty savedFeats then
                        \c r -> columnToRecordToText c r |> Html.Styled.fromUnstyled

                     else
                        savedFeats
                            |> Array.toList
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
