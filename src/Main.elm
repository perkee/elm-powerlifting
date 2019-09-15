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
import Bootstrap.Table as Table
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
import Css exposing (after)
import Data.ColumnToggles as ColumnToggles
import Dropdowns exposing (Option, typedSelect)
import Feat exposing (Feat, MassUnit, liftToString, testFeats)
import Html exposing (Html, div, h1, h2, span, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick, onInput)
import Html.Styled
import Html.Styled.Attributes as HSA
import Html.Styled.Keyed
import Library exposing (dropNothing, removeAt, stringToAttr, thrush, updateArrayAt)
import LiftForm
import Renderer exposing (floatToString, icon, rowsToHeadedTable)
import Scores
    exposing
        ( Record
        , featToRecord
        , maxRecord
        )
import View.ColumnToggles as ColumnToggles


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


type alias SavedFeat =
    { feat : Feat
    , index : Int
    , key : Int
    }


type alias Model =
    { formState : LiftForm.State
    , feats : Array SavedFeat
    , featKey : Int
    , featState : ColumnToggles.State
    , tableState : ColumnToggles.State
    , deleteConfirmVisibility : Modal.Visibility
    , idxToDelete : Maybe Int
    , sortColumn : SortColumn
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
                someFeats

            else
                Array.empty
      , featKey = 0
      , featState = ColumnToggles.init initCurrentColumns
      , tableState = ColumnToggles.init initTableColumns
      , deleteConfirmVisibility = Modal.hidden
      , idxToDelete = Nothing
      , sortOrder = Ascending
      , sortColumn = Index
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
    | ColumnHeaderArrowsClicked SortColumn
    | SortColumnDropdownChanged (Maybe SortColumn)
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
            [ h1 [] [ text "Every Score Calculator" ]
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
            , if Array.isEmpty model.feats then
                text ""

              else
                Grid.row
                    [ Row.attrs
                        [ style "margin-bottom" ".75rem"
                        , class "d-md-none"
                        ]
                    ]
                    [ model.tableState
                        |> ColumnToggles.columns
                        |> List.map columnToSortColumn
                        |> (::) (Just Index)
                        |> List.foldr
                            (\msc acc ->
                                case msc of
                                    Just sc ->
                                        Option sc
                                            (sortColumnToString sc)
                                            (sortColumnToString sc)
                                            :: acc

                                    Nothing ->
                                        acc
                            )
                            []
                        |> typedSelect [ Select.small ]
                        |> thrush model.sortColumn
                        |> thrush SortColumnDropdownChanged
                        |> List.singleton
                        |> Grid.col [ Col.xs10 ]
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
                    |> savedFeatsToLiftCards
                        (ColumnToggles.columns model.tableState)
                        model.liftCardUnits
                    |> Card.keyedColumns
                    |> List.singleton
                    |> Grid.col [ Col.xs12, Col.attrs [ class "d-md-none" ] ]
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


savedFeatsToColumnToRecordToText : Array SavedFeat -> Column -> Record -> Html msg
savedFeatsToColumnToRecordToText savedFeats =
    if Array.isEmpty savedFeats then
        columnToRecordToText

    else
        savedFeats
            |> Array.toList
            |> List.map (.feat >> featToRecord)
            |> maxRecord
            |> columnToRecordToTextWithMaxes


maybeMaxRecord : Array SavedFeat -> Maybe Record
maybeMaxRecord savedFeats =
    if Array.isEmpty savedFeats then
        Nothing

    else
        savedFeats
            |> Array.toList
            |> List.map (.feat >> featToRecord)
            |> maxRecord
            |> Just


savedFeatsToLiftCards : List Column -> MassUnit -> Array SavedFeat -> List ( String, Card.Config Msg )
savedFeatsToLiftCards cols liftCardsMassUnit savedFeats =
    let
        colToRecordToText =
            -- savedFeatsToColumnToRecordToText savedFeats
            columnToRecordToText
    in
    cols
        |> List.foldr
            (\col acc ->
                case columnToSortColumn col of
                    Just sc ->
                        ( col, sc ) :: acc

                    Nothing ->
                        acc
            )
            []
        |> List.map (columnToLiftCard savedFeats liftCardsMassUnit colToRecordToText)


featToSummaryPounds : Feat -> String
featToSummaryPounds f =
    floatToString f.liftedPounds ++ " @ " ++ floatToString f.bodyPounds


featToSummaryKilos : Feat -> String
featToSummaryKilos f =
    floatToString f.liftedKilos ++ " @ " ++ floatToString f.bodyKilos


savedFeatToRowForLiftCard : ( Column, SortColumn ) -> Bool -> MassUnit -> (Record -> Html Msg) -> SavedFeat -> ( String, Html Msg )
savedFeatToRowForLiftCard ( col, sortCol ) shouldShowLift liftCardsUnit recordToText savedFeat =
    let
        key =
            String.fromInt savedFeat.key ++ (sortCol |> sortColumnToString)
    in
    ( key
    , Html.Styled.Keyed.node "tr"
        [ HSA.class "lift-card__data-row"
        , HSA.css
            [ Css.after
                [ Css.display Css.block
                , Css.property "content"
                    "''"
                , Css.height <|
                    Css.px 3
                , Css.position
                    Css.absolute
                , Css.backgroundColor <|
                    Css.hex "000"
                , Css.width <|
                    Css.pct 100
                , Css.left Css.zero
                ]
            ]
        ]
        ([ ( key ++ "idx"
           , Html.Styled.th [ HSA.class "index" ]
                [ savedFeat.index |> String.fromInt |> Html.Styled.text ]
           )
         , ( key ++ "note", Html.Styled.td [ HSA.class "note" ] [ savedFeatToNoteInput "lift-card" savedFeat |> Html.Styled.fromUnstyled ] )
         ]
            ++ (if shouldShowLift then
                    [ ( key ++ "lift", Html.Styled.td [ HSA.class "lift" ] [ savedFeat.feat |> .lift |> liftToString |> text |> Html.Styled.fromUnstyled ] ) ]

                else
                    []
               )
            ++ [ case liftCardsUnit of
                    Feat.LBM ->
                        ( key ++ "summary--lb", Html.Styled.td [ HSA.class "summary summary--lb" ] [ featToSummaryPounds savedFeat.feat |> text |> Html.Styled.fromUnstyled ] )

                    Feat.KG ->
                        ( key ++ "summary--kg", Html.Styled.td [ HSA.class "summary summary--kg" ] [ featToSummaryKilos savedFeat.feat |> text |> Html.Styled.fromUnstyled ] )
               , ( key ++ "score", Html.Styled.td [ HSA.class "value" ] [ featToRecord savedFeat.feat |> recordToText |> Html.Styled.fromUnstyled ] )
               ]
        )
        |> Html.Styled.toUnstyled
    )


showLift : Array SavedFeat -> Bool
showLift savedFeats =
    case Array.toList savedFeats of
        [] ->
            Debug.log "empty savedfeats" False

        feat :: [] ->
            Debug.log "singleton savedfeats" False

        feat :: feats ->
            feats |> List.foldl (\f anyDiff -> Debug.log "returning" (anyDiff || Debug.log "current feat " (f.feat |> .lift) /= Debug.log "reference feat " (feat.feat |> .lift))) False


columnToLiftCard : Array SavedFeat -> MassUnit -> (Column -> Record -> Html Msg) -> ( Column, SortColumn ) -> ( String, Card.Config Msg )
columnToLiftCard savedFeats liftCardsUnit columnToRecordToText ( col, sortCol ) =
    let
        label =
            sortCol |> sortColumnToString

        rows =
            savedFeats
                |> sortSavedFeats Descending sortCol
                |> Array.map
                    (savedFeatToRowForLiftCard
                        ( col, sortCol )
                        (showLift savedFeats)
                        liftCardsUnit
                        (columnToRecordToText col)
                    )
                |> Array.toList
    in
    ( label
    , Card.config
        [ Card.attrs [ class "lift-card" ]
        ]
        |> Card.headerH4 []
            [ text label
            ]
        |> Card.block [ Block.attrs [ class "lift-card__block" ] ]
            [ Block.custom <|
                rowsToHeadedTable
                    ([ ( "", text "" )
                     , ( "Note", text "" )
                     ]
                        ++ (if showLift savedFeats then
                                [ ( "Lift", text "" ) ]

                            else
                                []
                           )
                        ++ [ ( ""
                             , Button.button
                                [ Button.outlineSecondary
                                , Button.onClick LiftCardUnitsToggleClicked
                                , Button.small
                                , Button.block
                                ]
                                [ text <|
                                    case liftCardsUnit of
                                        Feat.KG ->
                                            "Kg"

                                        Feat.LBM ->
                                            "Lb."
                                ]
                             )
                           , ( "Value", text "" )
                           ]
                    )
                    rows
            ]
    )


savedFeatsToCards : List Column -> Array SavedFeat -> List ( String, Card.Config Msg )
savedFeatsToCards cols savedFeats =
    savedFeats |> Array.map (savedFeatToCard cols savedFeats) |> Array.toList


savedFeatsToTable : SortColumn -> SortOrder -> List Column -> Array SavedFeat -> Html Msg
savedFeatsToTable sort order cols savedFeats =
    savedFeats
        |> Array.map (savedFeatToRow cols <| maxSavedFeat savedFeats)
        >> Array.toList
        >> ([ [ ( "Index"
                , icon
                    (case ( sort, order ) of
                        ( Index, Ascending ) ->
                            "sort-up"

                        ( Index, Descending ) ->
                            "sort-down"

                        ( _, _ ) ->
                            "sort"
                    )
                    [ onClick <| ColumnHeaderArrowsClicked Index
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


classToHtmlToCell : String -> Html Msg -> ( String, Table.Cell Msg )
classToHtmlToCell className html =
    ( className, Table.td [ className |> stringToAttr |> class |> Table.cellAttr ] [ html ] )


classToHtmlToStyledCell : String -> Html Msg -> ( String, Html.Styled.Html Msg )
classToHtmlToStyledCell className html =
    ( className, Html.Styled.td [ className |> stringToAttr |> HSA.class ] [ Html.Styled.fromUnstyled html ] )


columnToFloatToCell : Record -> Column -> Record -> ( String, Table.Cell Msg )
columnToFloatToCell maxes col =
    columnToRecordToTextWithMaxes maxes col >> classToHtmlToCell (col |> columnToColumnLabel |> (++) "body-cell--")


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


type SortColumn
    = BodyMass
    | LiftedMass
    | Wilks
    | ScaledAllometricIpf
    | ScaledAllometricAtr
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
            .feat >> .liftedKilos >> Just

        Wilks ->
            .feat >> featToRecord >> .wilks

        ScaledAllometricIpf ->
            .feat >> featToRecord >> .scaledAllometricIpf

        ScaledAllometricAtr ->
            .feat >> featToRecord >> .scaledAllometricAtr

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
    case ( sort, columnToSortColumn column ) of
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

        Column.ScaledAllometricIpf ->
            Just ScaledAllometricIpf

        Column.ScaledAllometricAtr ->
            Just ScaledAllometricAtr

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

        Column.Equipment ->
            Nothing


sortColumnToString : SortColumn -> String
sortColumnToString sc =
    case sc of
        BodyMass ->
            "Body Mass"

        LiftedMass ->
            "Lifted Mass"

        Wilks ->
            "Wilks"

        ScaledAllometricIpf ->
            "Scaled Allometric IPF"

        ScaledAllometricAtr ->
            "Scaled Allometric ATR"

        Allometric ->
            "Allometric"

        IPF ->
            "IPF"

        McCulloch ->
            "McCulloch"

        Index ->
            "Index"
