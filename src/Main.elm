module Main exposing (main)

-- (Html, button, div, text, input, option, select)
-- imports used

import Array exposing (Array)
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import Browser
import Column
    exposing
        ( Column
        , columnToColumnLabel
        , columnToRecordToText
        , columnToRecordToTextWithMaxes
        , columnToToggleLabel
        , initCurrentColumns
        , initTableColumns
        )
import Data.ColumnToggles as ColumnToggles
import Dropdowns exposing (Option, typedSelect)
import Feat exposing (Feat, MassUnit, testFeats)
import Html exposing (Html, div, h1, h2, h3, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick, onInput)
import Html.Styled
import Html.Styled.Attributes as HSA
import Html.Styled.Keyed
import Library exposing (dropNothing, removeAt, stringToAttr, thrush, updateArrayAt)
import LiftForm
import Renderer exposing (icon, rowsToHeadedTable)
import SavedFeat exposing (SavedFeat)
import Scores
    exposing
        ( Record
        , featToRecord
        , maxRecord
        )
import SortColumn
import View.ColumnToggles as ColumnToggles
import View.ScoreCards as ScoreCards


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
    , sortOrder : SortOrder
    , liftCardUnits : MassUnit
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
      , sortOrder = Ascending
      , sortColumn = SortColumn.Index
      , liftCardUnits = Feat.KG
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
                ++ (if Array.isEmpty model.feats then
                        []

                    else
                        ScoreCards.view (Array.toList model.feats) model.tableState model.liftCardUnits LiftCardUnitsToggleClicked NoteChanged
                            ++ [ h3 [ class "d-md-none" ] [ text "Grouped by Lifter" ]
                               , Grid.row
                                    [ Row.attrs
                                        [ style "margin-bottom" ".75rem"
                                        , class "d-md-none"
                                        ]
                                    ]
                                    [ Grid.col [ Col.xs2 ] [ text "Sort by:" ]
                                    , model.tableState
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
                                        |> thrush model.sortColumn
                                        |> thrush SortColumnDropdownChanged
                                        |> List.singleton
                                        |> Grid.col [ Col.xs8 ]
                                    , icon
                                        (case model.sortOrder of
                                            Ascending ->
                                                "sort-amount-down-alt"

                                            Descending ->
                                                "sort-amount-up"
                                        )
                                        []
                                        |> List.singleton
                                        |> Button.button
                                            [ Button.outlineSecondary
                                            , Button.onClick SortOrderToggleClicked
                                            , Button.small
                                            , Button.block
                                            ]
                                        |> List.singleton
                                        |> Grid.col [ Col.xs2 ]
                                    ]
                               , Grid.row []
                                    [ model.feats
                                        |> sortSavedFeats model.sortOrder model.sortColumn
                                        |> savedFeatsToCards
                                            (ColumnToggles.columns model.tableState)
                                        |> Card.keyedColumns
                                        |> List.singleton
                                        |> Grid.col [ Col.xs12, Col.attrs [ class "d-md-none" ] ]
                                    ]
                               ]
                   )
            )
        , Grid.containerFluid [ class "d-none d-md-block" ]
            [ Grid.row []
                [ Grid.col [ Col.sm12 ]
                    [ model.feats
                        |> sortSavedFeats model.sortOrder model.sortColumn
                        |> savedFeatsToTable
                            model.sortColumn
                            model.sortOrder
                            (ColumnToggles.columns model.tableState)
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


maxSavedFeat : Array SavedFeat -> Record
maxSavedFeat =
    Array.toList >> List.map (.feat >> featToRecord) >> maxRecord


savedFeatsToCards : List Column -> Array SavedFeat -> List ( String, Card.Config Msg )
savedFeatsToCards cols savedFeats =
    savedFeats |> Array.map (savedFeatToCard cols savedFeats) |> Array.toList


savedFeatsToTable : SortColumn.SortColumn -> SortOrder -> List Column -> Array SavedFeat -> Html Msg
savedFeatsToTable sort order cols savedFeats =
    savedFeats
        |> Array.map (savedFeatToRow cols <| maxSavedFeat savedFeats)
        >> Array.toList
        >> ([ [ ( "Index"
                , icon
                    (case ( sort, order ) of
                        ( SortColumn.Index, Ascending ) ->
                            "sort-up"

                        ( SortColumn.Index, Descending ) ->
                            "sort-down"

                        ( _, _ ) ->
                            "sort"
                    )
                    [ onClick <| ColumnHeaderArrowsClicked SortColumn.Index
                    ]
                )
              ]
            , [ ( "Note", text "" ) ]
            , List.map
                (\c -> ( columnToColumnLabel c, columnAndSortToIcon sort order c ))
                cols
            , [ ( "Delete", text "" ) ]
            ]
                |> List.concat
                |> rowsToHeadedTable
           )


classToHtmlToStyledCell : String -> Html Msg -> ( String, Html.Styled.Html Msg )
classToHtmlToStyledCell className html =
    ( className, Html.Styled.td [ className |> stringToAttr |> HSA.class ] [ Html.Styled.fromUnstyled html ] )


columnToFloatToStyledCell : Record -> Column -> Record -> ( String, Html.Styled.Html Msg )
columnToFloatToStyledCell maxes col =
    columnToRecordToTextWithMaxes maxes col >> classToHtmlToStyledCell (col |> columnToColumnLabel |> (++) "body-cell--")


savedFeatToNoteInput : String -> SavedFeat -> Html Msg
savedFeatToNoteInput classSuffix savedFeat =
    Input.text
        [ Input.placeholder "Note"
        , Input.value <| .note <| savedFeat.feat
        , Input.onInput <| NoteChanged savedFeat.index
        , Input.attrs [ class <| "note-input note-input--" ++ classSuffix ]
        ]


savedFeatToCard : List Column -> Array SavedFeat -> SavedFeat -> ( String, Card.Config Msg )
savedFeatToCard cols feats savedFeat =
    ( savedFeat.key |> String.fromInt
    , Card.config
        [ Card.attrs []
        ]
        |> Card.headerH4 []
            [ text <| String.fromInt <| savedFeat.index
            , Button.button
                [ Button.outlineDanger
                , DeleteButtonClicked savedFeat.index |> Button.onClick
                , Button.attrs [ class "card-delete" ]
                ]
                [ icon "trash" []
                ]
            , savedFeatToNoteInput "card" savedFeat
            ]
        |> Card.block []
            [ Block.custom <| featToTable feats cols savedFeat.feat ]
    )


savedFeatToRow : List Column -> Record -> SavedFeat -> ( String, Html Msg )
savedFeatToRow cols maxes savedFeat =
    [ [ .index >> String.fromInt >> text >> classToHtmlToStyledCell "body-cell--index"
      , savedFeatToNoteInput "table"
            >> classToHtmlToStyledCell "body-cell--note"
      ]
        |> List.map (thrush savedFeat)
    , (savedFeat.feat
        |> featToRecord
        |> thrush
        |> List.map
      )
        (List.map (columnToFloatToStyledCell maxes) cols)
    , Button.button
        [ Button.outlineDanger
        , DeleteButtonClicked savedFeat.index |> Button.onClick
        ]
        [ icon "trash" []
        ]
        |> classToHtmlToStyledCell "body-cell--delete"
        |> List.singleton
    ]
        |> List.concat
        |> Html.Styled.Keyed.node "tr" []
        |> (\row -> ( savedFeat.key |> String.fromInt, row |> Html.Styled.toUnstyled ))


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


type SortOrder
    = Ascending
    | Descending


sortByOrder : SortOrder -> comparable -> comparable -> Order
sortByOrder sortOrder a b =
    case ( sortOrder, compare a b ) of
        ( Ascending, anyOrder ) ->
            anyOrder

        ( Descending, LT ) ->
            GT

        ( Descending, GT ) ->
            LT

        ( Descending, EQ ) ->
            EQ


sortColumnToGetter : SortColumn.SortColumn -> SavedFeat -> Maybe Float
sortColumnToGetter col =
    case col of
        SortColumn.BodyMass ->
            .feat >> .bodyKilos >> Just

        SortColumn.LiftedMass ->
            .feat >> .liftedKilos >> Just

        SortColumn.Wilks ->
            .feat >> featToRecord >> .wilks

        SortColumn.ScaledAllometricIpf ->
            .feat >> featToRecord >> .scaledAllometricIpf

        SortColumn.ScaledAllometricAtr ->
            .feat >> featToRecord >> .scaledAllometricAtr

        SortColumn.Allometric ->
            .feat >> featToRecord >> .allometric

        SortColumn.IPF ->
            .feat >> featToRecord >> .ipf

        SortColumn.McCulloch ->
            .feat >> featToRecord >> .mcCulloch

        SortColumn.Index ->
            .index >> toFloat >> Just


sortSavedFeats : SortOrder -> SortColumn.SortColumn -> Array SavedFeat -> Array SavedFeat
sortSavedFeats sortOrder sortColumn =
    Array.toList
        >> List.sortWith
            (\a b ->
                sortByOrder sortOrder
                    (a |> sortColumnToGetter sortColumn |> Maybe.withDefault (-1 / 0))
                    (b |> sortColumnToGetter sortColumn |> Maybe.withDefault (-1 / 0))
            )
        >> Array.fromList


columnAndSortToIcon : SortColumn.SortColumn -> SortOrder -> Column -> Html Msg
columnAndSortToIcon sort order column =
    case ( sort, SortColumn.fromColumn column ) of
        ( s, Just sc ) ->
            (if s == sc then
                case order of
                    Ascending ->
                        "sort-up"

                    Descending ->
                        "sort-down"

             else
                "sort"
            )
                |> icon
                |> thrush
                    [ class "sort-button"
                    , onClick <| ColumnHeaderArrowsClicked sc
                    ]

        ( _, Nothing ) ->
            text ""
