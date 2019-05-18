module Main exposing (main)

-- (Html, button, div, text, input, option, select)
-- imports used

import Array exposing (Array)
import Bootstrap.Accordion as Accordion
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
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
        , allColumns
        , columnToColumnLabel
        , columnToRecordToText
        , columnToRecordToTextWithMaxes
        , columnToToggleLabel
        , initCurrentColumns
        , initTableColumns
        )
import Dropdowns exposing (Option, typedSelect)
import Feat exposing (Equipment(..), Feat, Gender(..), Lift(..), MassUnit(..), genderToString, massToKilos, massToPounds)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onCheck, onClick, onInput, targetValue)
import Json.Decode as Json
import Json.Encode as JE
import Library exposing (filterListByList, removeAt, stringToAttr, thrush, updateArrayAt)
import Renderer exposing (htmlsToRow, rowsToHeadedTable, textual)
import Scores
    exposing
        ( Record
        , featToRecord
        , maxRecord
        )


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Dropdown.subscriptions model.liftedUnitState SetLiftedUnitState
        , Dropdown.subscriptions model.bodyUnitState SetBodyUnitState
        , Accordion.subscriptions model.tableAccordionState SetTableAccordion
        , Accordion.subscriptions model.currentAccordionState SetCurrentAccordion
        , Modal.subscriptions model.deleteConfirmVisibility AnimateDeleteModal
        ]


type alias FloatField =
    { value : Maybe Float
    , input : String
    }


type alias SavedFeat =
    { feat : Feat
    , index : Int
    , note : String
    , key : Int
    }


initFloatField : FloatField
initFloatField =
    { value = Nothing
    , input = ""
    }


type alias Model =
    { liftedMass : FloatField
    , liftedUnit : MassUnit
    , liftedUnitState : Dropdown.State
    , bodyMass : FloatField
    , bodyUnit : MassUnit
    , bodyUnitState : Dropdown.State
    , gender : Gender
    , lift : Lift
    , age : FloatField
    , feats : Array SavedFeat
    , featKey : Int
    , currentColumns : List Column
    , tableColumns : List Column
    , equipment : Equipment
    , currentAccordionState : Accordion.State
    , tableAccordionState : Accordion.State
    , deleteConfirmVisibility : Modal.Visibility
    , idxToDelete : Maybe Int
    }


someFeats : Array SavedFeat
someFeats =
    Array.fromList
        [ { index = 0
          , key = -3
          , note = "first"
          , feat =
                { bodyKilos = 100
                , bodyPounds = 220
                , liftedKilos = 400
                , liftedPounds = 881
                , gender = Male
                , lift = Total
                , age = Just 40
                }
          }
        , { index = 1
          , key = -2
          , note = "second"
          , feat =
                { bodyKilos = 70
                , bodyPounds = 154
                , liftedKilos = 500
                , liftedPounds = 1101
                , gender = Male
                , lift = Total
                , age = Just 45
                }
          }
        , { index = 2
          , key = -1
          , note = "third"
          , feat =
                { bodyKilos = 60
                , bodyPounds = 132
                , liftedKilos = 505
                , liftedPounds = 1112
                , gender = Male
                , lift = Total
                , age = Just 50
                }
          }
        ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( { liftedMass = initFloatField
      , liftedUnit = KG
      , liftedUnitState = Dropdown.initialState
      , bodyMass = initFloatField
      , bodyUnit = KG
      , bodyUnitState = Dropdown.initialState
      , gender = GNC
      , lift = Total
      , age = initFloatField
      , feats = Array.empty -- someFeats --
      , featKey = 0
      , currentColumns = initCurrentColumns
      , tableColumns = initTableColumns
      , equipment = Raw
      , tableAccordionState = Accordion.initialState
      , currentAccordionState = Accordion.initialState
      , deleteConfirmVisibility = Modal.hidden
      , idxToDelete = Nothing
      }
    , Cmd.none
    )


modelToFeat : Model -> Maybe Feat
modelToFeat m =
    case ( m.bodyMass.value, m.liftedMass.value ) of
        ( Just bodyMass, Just liftedMass ) ->
            Just
                { bodyKilos = massToKilos m.bodyUnit bodyMass
                , bodyPounds = massToPounds m.bodyUnit bodyMass
                , liftedKilos = massToKilos m.liftedUnit liftedMass
                , liftedPounds = massToPounds m.liftedUnit liftedMass
                , gender = m.gender
                , lift = m.lift
                , age = m.age.value
                }

        ( _, _ ) ->
            Nothing


modelToRecord : Model -> Maybe Record
modelToRecord model =
    model |> modelToFeat |> Maybe.map featToRecord


canMakeFeat : Maybe Feat -> Bool
canMakeFeat m =
    case m of
        Just _ ->
            True

        Nothing ->
            False


type Msg
    = NoMsg
    | SetLiftedMass String
    | SetLiftedUnit MassUnit
    | SetLiftedUnitState Dropdown.State
    | SetBodyMass String
    | SetBodyUnit MassUnit
    | SetBodyUnitState Dropdown.State
    | SetGender (Maybe Gender)
    | SetLift (Maybe Lift)
    | SetAge String
    | SaveFeat
    | SetNote Int String
    | DeleteRow Int
    | ToggleTableColumn Column Bool
    | ToggleCurrentColumn Column Bool
    | SetEquipment (Maybe Equipment)
    | SetTableAccordion Accordion.State
    | SetCurrentAccordion Accordion.State
    | AnimateDeleteModal Modal.Visibility
    | AskDelete Int
    | CancelDelete
    | ConfirmDelete


ffParse : String -> FloatField
ffParse str =
    case String.toFloat str of
        Nothing ->
            { value = Nothing, input = str }

        Just new ->
            { value = Just new, input = str }


setNoteOnSavedFeat : String -> SavedFeat -> SavedFeat
setNoteOnSavedFeat note feat =
    { feat | note = note }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        NoMsg ->
            model

        SetLiftedMass s ->
            { model | liftedMass = ffParse s }

        SetLiftedUnit u ->
            { model | liftedUnit = u }

        SetLiftedUnitState state ->
            { model | liftedUnitState = state }

        SetBodyMass s ->
            { model | bodyMass = ffParse s }

        SetBodyUnit u ->
            { model | bodyUnit = u }

        SetBodyUnitState state ->
            { model | bodyUnitState = state }

        SetGender mg ->
            case mg of
                Just g ->
                    { model | gender = g }

                Nothing ->
                    model

        SetLift ml ->
            case ml of
                Just l ->
                    { model | lift = l }

                Nothing ->
                    model

        SetAge s ->
            { model | age = ffParse s }

        SaveFeat ->
            case model |> modelToFeat of
                Just feat ->
                    { model
                        | feats = Array.push (SavedFeat feat (Array.length model.feats) "" model.featKey) model.feats
                        , featKey = model.featKey + 1
                    }

                Nothing ->
                    model

        SetNote index note ->
            { model | feats = updateArrayAt index (setNoteOnSavedFeat note) model.feats }

        DeleteRow idx ->
            { model | feats = removeAt model.feats idx }

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

        SetEquipment me ->
            case me of
                Just e ->
                    { model | equipment = e }

                Nothing ->
                    model

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
                            removeAt model.feats idx

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


saveButton : Bool -> Html Msg
saveButton canSave =
    Button.button
        [ Button.onClick SaveFeat
        , if canSave then
            Button.success

          else
            Button.secondary
        , Button.block
        , canSave |> not |> Button.disabled
        , Button.small
        ]
        [ text "Add to Table"
        ]


unitDropDown : Dropdown.State -> (Dropdown.State -> Msg) -> MassUnit -> (MassUnit -> Msg) -> InputGroup.Addon Msg
unitDropDown state stateMsg massUnit unitMsg =
    InputGroup.dropdown
        state
        { options = []
        , toggleMsg = stateMsg
        , toggleButton =
            (case massUnit of
                KG ->
                    "Kilos"

                LBM ->
                    "Pounds"
            )
                |> text
                |> List.singleton
                |> Dropdown.toggle [ Button.primary, Button.small ]
        , items =
            [ Dropdown.buttonItem [ KG |> unitMsg |> onClick ] [ text "Kilos" ]
            , Dropdown.buttonItem [ LBM |> unitMsg |> onClick ] [ text "Pounds" ]
            ]
        }


lifterForm : Model -> Html Msg
lifterForm model =
    Form.form []
        [ h2 [] [ text "Lift input" ]
        , Form.row [ Row.attrs [ class " mb-0" ] ]
            [ Form.col [ Col.xs12, Col.md6 ]
                [ Form.row []
                    [ Form.colLabel [ Col.xs4, Col.sm3 ] [ text "Gender" ]
                    , Form.col [ Col.xs8, Col.sm9 ]
                        [ typedSelect [ Select.small ]
                            [ Option Male "Male" "M"
                            , Option Female "Female" "F"
                            , Option GNC "—" "GNC"
                            ]
                            model.gender
                            SetGender
                        ]
                    ]
                ]
            , Form.col [ Col.xs12, Col.md6 ]
                [ Form.row []
                    [ Form.colLabel [ Col.xs4, Col.sm3 ] [ text "Event" ]
                    , Form.col [ Col.xs8, Col.sm9 ]
                        [ typedSelect [ Select.small ]
                            [ Option Total "Total" "T"
                            , Option Squat "Squat" "S"
                            , Option Bench "Bench" "B"
                            , Option Deadlift "Deadlift" "D"
                            ]
                            model.lift
                            SetLift
                        ]
                    ]
                ]
            ]
        , Form.row [ Row.attrs [ class " mb-0" ] ]
            [ Form.col [ Col.xs12, Col.md6 ]
                [ Form.row []
                    [ Form.colLabel [ Col.xs4, Col.sm3 ] [ text "Lifted weight" ]
                    , Form.col [ Col.xs8, Col.sm9 ]
                        [ InputGroup.config
                            (InputGroup.number
                                [ Input.placeholder "0"
                                , Input.onInput SetLiftedMass
                                , Input.attrs [ pattern "\\d+(\\.\\d+)?", attribute "inputmode" "decimal" ]
                                ]
                            )
                            |> InputGroup.successors
                                [ unitDropDown
                                    model.liftedUnitState
                                    SetLiftedUnitState
                                    model.liftedUnit
                                    SetLiftedUnit
                                ]
                            |> InputGroup.small
                            |> InputGroup.view
                        ]
                    ]
                ]
            , Form.col [ Col.xs12, Col.md6 ]
                [ Form.row []
                    [ Form.colLabel [ Col.xs4, Col.sm3 ] [ text "Bodyweight" ]
                    , Form.col [ Col.xs8, Col.sm9 ]
                        [ InputGroup.config
                            (InputGroup.number
                                [ Input.placeholder "0"
                                , Input.onInput SetBodyMass
                                , Input.attrs [ pattern "\\d+(\\.\\d+)?", attribute "inputmode" "decimal" ]
                                ]
                            )
                            |> InputGroup.successors
                                [ unitDropDown
                                    model.bodyUnitState
                                    SetBodyUnitState
                                    model.bodyUnit
                                    SetBodyUnit
                                ]
                            |> InputGroup.small
                            |> InputGroup.view
                        ]
                    ]
                ]
            ]
        , Form.row [ Row.attrs [ class " mb-0" ] ]
            -- [ Form.col [ Col.xs12, Col.md6 ]
            --     [ Form.row []
            --         [ Form.colLabel [ Col.xs4, Col.sm3 ] [ text "Equipment" ]
            --         , Form.col [ Col.xs8, Col.sm9 ]
            --             [ typedSelect [ Select.disabled True, Select.small ]
            --                 [ Option Raw "raw" "R"
            --                 , Option SinglePly "single ply" "SP"
            --                 ]
            --                 model.equipment
            --                 SetEquipment
            --             ]
            --         ]
            --     ]
            [ Form.col [ Col.xs12, Col.md6 ]
                [ Form.row []
                    [ Form.col [ Col.xs7, Col.sm7 ]
                        [ Form.row []
                            [ Form.colLabel [ Col.xs7, Col.sm5 ] [ text "Age" ]
                            , Form.col [ Col.xs5, Col.sm7 ]
                                [ Input.number
                                    [ Input.placeholder "0"
                                    , Input.onInput SetAge
                                    , Input.attrs [ pattern "\\d+(\\.\\d+)?", attribute "inputmode" "decimal" ]
                                    , Input.value model.age.input
                                    , Input.small
                                    ]
                                ]
                            ]
                        ]
                    , Form.col [ Col.xs5, Col.sm5 ]
                        [ model |> modelToFeat |> canMakeFeat |> saveButton
                        ]
                    ]
                ]
            ]
        ]


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
    div []
        [ Grid.container []
            [ h1 [] [ text "Every Score Calculator" ]
            , lifterForm model
            , h2 [] [ text "Current Score" ]
            , Grid.row []
                [ Grid.col [ Col.xs12 ]
                    (case model |> modelToFeat of
                        Just feat ->
                            [ accordion
                                model.currentAccordionState
                                SetCurrentAccordion
                                "current-column-toggles"
                                model.currentColumns
                                ToggleCurrentColumn
                                "Current Scores Options"
                            , featToTable model.currentColumns feat
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
                    [ model.feats |> savedFeatsToTable (filterListByList model.tableColumns allColumns) ]
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

                    -- todo: animate this on confirm. Gets into an infinite loop rn. Kinda impresseive!
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


savedFeatsToTable : List Column -> Array SavedFeat -> Html Msg
savedFeatsToTable cols savedFeats =
    let
        maxes =
            savedFeats |> Array.toList |> List.map (.feat >> featToRecord) |> maxRecord
    in
    savedFeats
        |> Array.indexedMap (savedFeatToRow cols maxes)
        >> Array.toList
        >> ([ [ "Index" ]
            , [ "Note" ]
            , List.map
                columnToColumnLabel
                cols
            , [ "Delete" ]
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


savedFeatToRow : List Column -> Record -> Int -> SavedFeat -> ( String, Table.Row Msg )
savedFeatToRow cols maxes index savedFeat =
    [ [ .index >> String.fromInt >> text >> classToHtmlToCell "body-cell--index"
      , .note
            >> (\v ->
                    Input.text
                        [ Input.placeholder "Note"
                        , Input.value v
                        , Input.onInput (SetNote index)
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

        -- , index |> DeleteRow |> Button.onClick
        , AskDelete index |> Button.onClick
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


featToTable : List Column -> Feat -> Html Msg
featToTable cols feat =
    (feat |> featToRecord |> thrush |> List.map) (List.map columnToRecordToText cols)
        |> List.map2
            (\label value ->
                ( label |> columnToToggleLabel
                , Table.tr []
                    [ label |> columnToToggleLabel |> text |> List.singleton |> Table.td []
                    , value |> List.singleton |> Table.td []
                    ]
                )
            )
            cols
        |> rowsToHeadedTable [ "Label", "Value" ]
