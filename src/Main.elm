module Main exposing (main)

-- (Html, button, div, text, input, option, select)
-- imports used

import Array exposing (Array)
import Bootstrap.Accordion as Accordion
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import Bootstrap.Table as Table
import Browser
import Column
    exposing
        ( Column
        , allColumns
        , columnToColumnLabel
        , columnToRecordToText
        , columnToRecordToTextWithMaxes
        , columnToToggleLabel
        , initCurrentColumns
        , initTableColumns
        )
import Data.LiftForm as LiftForm
import Feat exposing (Feat, testFeats)
import Html exposing (Html, div, h1, h2, span, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onCheck, onClick, onInput)
import Library exposing (filterListByList, removeAt, stringToAttr, thrush, updateArrayAt)
import Renderer exposing (rowsToHeadedTable)
import Scores
    exposing
        ( Record
        , featToRecord
        , maxRecord
        )
import View.LiftForm as LiftForm


main : Platform.Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ LiftForm.toSubs model.formState UpdateForm
        , Accordion.subscriptions model.tableAccordionState SetTableAccordion
        , Accordion.subscriptions model.currentAccordionState SetCurrentAccordion
        , Modal.subscriptions model.deleteConfirmVisibility AnimateDeleteModal
        ]


type alias SavedFeat =
    { feat : Feat
    , index : Int
    , note : String
    , key : Int
    }


type alias Model =
    { formState : LiftForm.State
    , feats : Array SavedFeat
    , featKey : Int
    , currentColumns : List Column
    , tableColumns : List Column
    , currentAccordionState : Accordion.State
    , tableAccordionState : Accordion.State
    , deleteConfirmVisibility : Modal.Visibility
    , idxToDelete : Maybe Int
    , sortColumn : SortColumn
    , sortOrder : SortOrder
    }


someFeats : Array SavedFeat
someFeats =
    testFeats
        |> List.map2 (\( index, key, note ) feat -> SavedFeat feat index note key)
            [ ( 0, 1, "first" )
            , ( 1, 2, "second" )
            , ( 2, 3, "third" )
            ]
        |> Array.fromList


init : () -> ( Model, Cmd Msg )
init _ =
    ( { formState = LiftForm.init
      , feats = Array.empty --someFeats
      , featKey = 4
      , currentColumns = initCurrentColumns
      , tableColumns = initTableColumns
      , tableAccordionState = Accordion.initialState
      , currentAccordionState = Accordion.initialState
      , deleteConfirmVisibility = Modal.hidden
      , idxToDelete = Nothing
      , sortOrder = Ascending
      , sortColumn = BodyMass
      }
    , Cmd.none
    )


modelToFeat : Model -> Maybe Feat
modelToFeat =
    .formState >> LiftForm.toFeat


type Msg
    = UpdateForm LiftForm.State
    | SaveFeat (Maybe Feat)
    | SetNote Int String
    | ToggleTableColumn Column Bool
    | ToggleCurrentColumn Column Bool
    | SetTableAccordion Accordion.State
    | SetCurrentAccordion Accordion.State
    | AnimateDeleteModal Modal.Visibility
    | AskDelete Int
    | CancelDelete
    | ConfirmDelete
    | SetSortColumn SortColumn


setNoteOnSavedFeat : String -> SavedFeat -> SavedFeat
setNoteOnSavedFeat note feat =
    { feat | note = note }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        UpdateForm state ->
            { model | formState = state }

        SaveFeat mf ->
            case mf of
                Just feat ->
                    { model
                        | feats = Array.push (SavedFeat feat (Array.length model.feats) "" model.featKey) model.feats
                        , featKey = model.featKey + 1
                    }

                Nothing ->
                    model

        SetNote index note ->
            { model | feats = updateArrayAt index (setNoteOnSavedFeat note) model.feats }

        ToggleTableColumn col checked ->
            let
                newColumns =
                    if checked then
                        col :: model.tableColumns

                    else
                        List.filter ((/=) col) model.tableColumns
            in
            { model | tableColumns = newColumns }

        ToggleCurrentColumn col checked ->
            let
                newColumns =
                    if checked then
                        col :: model.currentColumns

                    else
                        List.filter ((/=) col) model.currentColumns
            in
            { model | currentColumns = newColumns }

        SetTableAccordion state ->
            { model | tableAccordionState = state }

        SetCurrentAccordion state ->
            { model | currentAccordionState = state }

        CancelDelete ->
            { model
                | deleteConfirmVisibility = Modal.hidden
                , idxToDelete = Nothing
            }

        ConfirmDelete ->
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

        AskDelete idx ->
            { model
                | deleteConfirmVisibility = Modal.shown
                , idxToDelete = Just idx
            }

        AnimateDeleteModal visibility ->
            -- Just fadein
            { model | deleteConfirmVisibility = visibility }

        SetSortColumn column ->
            { model
                | sortColumn = column
                , sortOrder =
                    if model.sortColumn /= column then
                        Descending

                    else
                        case model.sortOrder of
                            Ascending ->
                                Descending

                            Descending ->
                                Ascending
            }
    , Cmd.none
    )


columnToToggle : String -> (Column -> Bool -> Msg) -> List Column -> Column -> Html Msg
columnToToggle prefix msg columns col =
    Checkbox.checkbox
        [ col
            |> columnToToggleLabel
            |> (++) prefix
            |> Checkbox.id
        , Checkbox.onCheck <|
            msg col
        , Checkbox.checked <| List.member col columns
        , Checkbox.inline
        ]
        (col |> columnToToggleLabel)


accordion : Accordion.State -> (Accordion.State -> Msg) -> String -> List Column -> (Column -> Bool -> Msg) -> String -> Html Msg
accordion state toggleAccordionMsg id columns toggleColumnMsg title =
    Accordion.config toggleAccordionMsg
        |> Accordion.withAnimation
        |> Accordion.cards
            [ Accordion.card
                { id = id
                , options = []
                , header =
                    Accordion.toggle [ class "btn-block" ]
                        [ span
                            [ (if Accordion.isOpen id state then
                                "fa fa-chevron-down"

                               else
                                "fa fa-chevron-up"
                              )
                                |> class
                            , style "float" "left"
                            , style "padding" ".125em 0 0 0"
                            ]
                            []
                        , text title
                        ]
                        |> Accordion.header [ style "padding" "0" ]
                , blocks =
                    [ Accordion.block []
                        [ Block.titleH4 [] [ text "Toggle Fields" ]
                        , Block.text []
                            [ Form.row
                                []
                                (allColumns
                                    |> List.map
                                        (columnToToggle id toggleColumnMsg columns
                                            >> List.singleton
                                            >> Form.col [ Col.xs6, Col.sm4, Col.md3, Col.lg3 ]
                                        )
                                )
                            ]
                        ]
                    ]
                }
            ]
        |> Accordion.view state


view : Model -> Html Msg
view model =
    let
        currentFeat =
            modelToFeat model
    in
    div []
        [ Grid.container []
            [ h1 [] [ text "Every Score Calculator" ]
            , LiftForm.view model.formState UpdateForm (SaveFeat (modelToFeat model))
            , h2 [] [ text "Current Score" ]
            , Grid.row [ Row.attrs [ class "current-table" ] ]
                [ Grid.col [ Col.xs12 ]
                    (case currentFeat of
                        Just feat ->
                            [ accordion
                                model.currentAccordionState
                                SetCurrentAccordion
                                "current-column-toggles"
                                model.currentColumns
                                ToggleCurrentColumn
                                "Current Scores Options"
                            , featToTable model.feats model.currentColumns feat
                            ]

                        Nothing ->
                            [ Alert.simpleInfo [] [ text "Enter data to see all scores for a lift" ] ]
                    )
                ]
            , h2 [] [ text "Scores Table" ]
            , if Array.isEmpty model.feats then
                Alert.simpleInfo [] [ text "Add scores to the table to compare" ]

              else
                accordion
                    model.tableAccordionState
                    SetTableAccordion
                    "table-column-toggles"
                    model.tableColumns
                    ToggleTableColumn
                    "Table Options"
            ]
        , Grid.containerFluid []
            [ Grid.row []
                [ Grid.col [ Col.sm12 ]
                    [ model.feats
                        |> sortSavedFeats model.sortOrder model.sortColumn
                        |> savedFeatsToTable
                            model.sortColumn
                            model.sortOrder
                            (filterListByList model.tableColumns allColumns)
                    ]
                ]
            ]
        , Modal.config CancelDelete
            |> Modal.withAnimation AnimateDeleteModal
            |> Modal.large
            |> Modal.h3 [] [ text "Confirm delete" ]
            |> Modal.body []
                [ Grid.containerFluid []
                    [ Grid.row []
                        [ Grid.col
                            [ Col.xs12 ]
                            [ "Are you sure you want to delete the entry at row "
                                ++ (case model.idxToDelete of
                                        Just idx ->
                                            String.fromInt idx

                                        Nothing ->
                                            "whaaaa???"
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
                    , Button.attrs [ onClick <| ConfirmDelete ]
                    ]
                    [ text "Yes, Delete this row" ]
                , Button.button
                    [ Button.outlineSuccess
                    , Button.attrs [ onClick <| CancelDelete ]
                    ]
                    [ text "Nevermind" ]
                ]
            |> Modal.view model.deleteConfirmVisibility
        ]


savedFeatsToTable : SortColumn -> SortOrder -> List Column -> Array SavedFeat -> Html Msg
savedFeatsToTable sort order cols savedFeats =
    let
        maxes =
            savedFeats |> Array.toList |> List.map (.feat >> featToRecord) |> maxRecord
    in
    savedFeats
        |> Array.map (savedFeatToRow cols maxes)
        >> Array.toList
        >> ([ [ ( "Index"
                , span
                    [ (case ( sort, order ) of
                        ( Index, Ascending ) ->
                            "fa-sort-up"

                        ( Index, Descending ) ->
                            "fa-sort-down"

                        ( _, _ ) ->
                            "fa-sort"
                      )
                        |> (++) "fa "
                        |> class
                    , onClick (SetSortColumn Index)
                    ]
                    []
                )
              ]
            , [ ( "Note", span [] [] ) ]
            , List.map
                (\c -> ( columnToColumnLabel c, columnAndSortToIcon sort order c ))
                cols
            , [ ( "Delete", span [] [] ) ]
            ]
                |> List.concat
                |> rowsToHeadedTable
           )


classToHtmlToCell : String -> Html Msg -> Table.Cell Msg
classToHtmlToCell className =
    List.singleton >> Table.td [ className |> stringToAttr |> class |> Table.cellAttr ]


columnToFloatToCell : Record -> Column -> Record -> Table.Cell Msg
columnToFloatToCell maxes col =
    columnToRecordToTextWithMaxes maxes col >> classToHtmlToCell (col |> columnToColumnLabel |> (++) "body-cell--")


savedFeatToRow : List Column -> Record -> SavedFeat -> ( String, Table.Row Msg )
savedFeatToRow cols maxes savedFeat =
    [ [ .index >> String.fromInt >> text >> classToHtmlToCell "body-cell--index"
      , .note
            >> (\v ->
                    Input.text
                        [ Input.placeholder "Note"
                        , Input.value v
                        , Input.onInput (SetNote savedFeat.index)
                        , Input.attrs [ class "note-input" ]
                        ]
               )
            >> classToHtmlToCell "body-cell--note"
      ]
        |> List.map (thrush savedFeat)
    , (savedFeat.feat
        |> featToRecord
        |> thrush
        |> List.map
      )
        (List.map (columnToFloatToCell maxes) cols)
    , Button.button
        [ Button.outlineDanger
        , AskDelete savedFeat.index |> Button.onClick
        ]
        [ span [ class "fa fa-trash" ] []
        ]
        |> List.singleton
        |> Table.td [ "body-cell--delete" |> class |> Table.cellAttr ]
        |> List.singleton
    ]
        |> List.concat
        |> Table.tr []
        |> (\row -> ( savedFeat.key |> String.fromInt, row ))


featToTable : Array SavedFeat -> List Column -> Feat -> Html Msg
featToTable savedFeats cols =
    let
        recordsToText =
            cols
                |> List.map
                    (if Array.isEmpty savedFeats then
                        columnToRecordToText

                     else
                        savedFeats
                            |> Array.toList
                            |> List.map (.feat >> featToRecord)
                            |> maxRecord
                            |> columnToRecordToTextWithMaxes
                    )
    in
    featToRecord
        >> thrush
        >> List.map
        >> thrush recordsToText
        >> List.map2
            (\label value ->
                ( label |> columnToToggleLabel
                , Table.tr []
                    [ label |> columnToToggleLabel |> text |> List.singleton |> Table.td [ "body-cell--label" |> class |> Table.cellAttr ]
                    , value |> List.singleton |> Table.td [ "body-cell--value" |> class |> Table.cellAttr ]
                    ]
                )
            )
            cols
        >> rowsToHeadedTable [ ( "Label", span [] [] ), ( "Value", span [] [] ) ]


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


type SortColumn
    = BodyMass
    | LiftedMass
    | Wilks
    | ScaledAllometric
    | Allometric
    | IPF
    | McCulloch
    | Index


sortColumnToGetter : SortColumn -> SavedFeat -> Maybe Float
sortColumnToGetter col =
    case col of
        BodyMass ->
            .feat >> .bodyKilos >> Just

        LiftedMass ->
            .feat >> .bodyKilos >> Just

        Wilks ->
            .feat >> featToRecord >> .wilks

        ScaledAllometric ->
            .feat >> featToRecord >> .scaledAllometric

        Allometric ->
            .feat >> featToRecord >> .allometric

        IPF ->
            .feat >> featToRecord >> .ipf

        McCulloch ->
            .feat >> featToRecord >> .mcCulloch

        Index ->
            .index >> toFloat >> Just


sortSavedFeats : SortOrder -> SortColumn -> Array SavedFeat -> Array SavedFeat
sortSavedFeats sortOrder sortColumn =
    Array.toList
        >> List.sortWith
            (\a b ->
                sortByOrder sortOrder
                    (a |> sortColumnToGetter sortColumn |> Maybe.withDefault (-1 / 0))
                    (b |> sortColumnToGetter sortColumn |> Maybe.withDefault (-1 / 0))
            )
        >> Array.fromList


columnAndSortToIcon : SortColumn -> SortOrder -> Column -> Html Msg
columnAndSortToIcon sort order column =
    let
        arrows =
            "fa "
                ++ (case order of
                        Ascending ->
                            "fa-sort-up"

                        Descending ->
                            "fa-sort-down"
                   )
    in
    (case ( sort, columnToSortColumn column ) of
        ( s, Just sc ) ->
            (if s == sc then
                arrows

             else
                "fa fa-sort"
            )
                |> class
                |> List.singleton
                |> (++)
                    [ class "sort-button"
                    , onClick (SetSortColumn sc)
                    ]

        ( _, Nothing ) ->
            [ class "fa" ]
    )
        |> span
        |> thrush []


columnToSortColumn : Column -> Maybe SortColumn
columnToSortColumn col =
    case col of
        Column.BodyKilos ->
            Just BodyMass

        Column.LiftedKilos ->
            Just LiftedMass

        Column.BodyPounds ->
            Just BodyMass

        Column.LiftedPounds ->
            Just LiftedMass

        Column.Wilks ->
            Just Wilks

        Column.ScaledAllometric ->
            Just ScaledAllometric

        Column.Allometric ->
            Just Allometric

        Column.IPF ->
            Just IPF

        Column.McCulloch ->
            Just McCulloch

        Column.Gender ->
            Nothing

        Column.Lift ->
            Nothing
