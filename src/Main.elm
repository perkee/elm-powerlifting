module Main exposing (main)

-- (Html, button, div, text, input, option, select)
-- imports used

import Array exposing (Array)
import Bootstrap.Accordion as Accordion
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
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
import Bootstrap.Table as Table
import Browser
import Column exposing (Column, columnToRecordToText, columnToToggleLabel, initColumns)
import Dropdowns exposing (Option, typedSelect)
import Feat exposing (Equipment(..), Feat, Gender(..), Lift(..), MassUnit(..), genderToString, massToKilos, massToPounds)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onCheck, onClick, onInput, targetValue)
import Json.Decode as Json
import Json.Encode as JE
import Library exposing (filterListByList, thrush, updateArrayAt)
import Renderer exposing (htmlsToRow, rowsToHeadedTable, textual)
import Scores
    exposing
        ( Record
        , featToRecord
        )


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Dropdown.subscriptions model.liftedUnitState SetLiftedUnitState
        , Accordion.subscriptions model.tableAccordionState SetTableAccordion
        ]


type alias FloatField =
    { value : Maybe Float
    , input : String
    }


type alias SavedFeat =
    { feat : Feat
    , index : Int
    , note : String
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
    , columns : List Column
    , equipment : Equipment
    , tableAccordionState : Accordion.State
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { liftedMass = initFloatField
      , liftedUnit = LBM
      , liftedUnitState = Dropdown.initialState
      , bodyMass = initFloatField
      , bodyUnit = LBM
      , bodyUnitState = Dropdown.initialState
      , gender = GNC
      , lift = Total
      , age = initFloatField
      , feats = Array.empty
      , columns = initColumns
      , equipment = Raw
      , tableAccordionState = Accordion.initialState
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
    = SetLiftedMass String
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
    | ToggleColumn Column Bool
    | SetEquipment (Maybe Equipment)
    | SetTableAccordion Accordion.State


ffValue : FloatField -> Float
ffValue ff =
    ff.value |> Maybe.withDefault 0


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
                    { model | feats = Array.push (SavedFeat feat (Array.length model.feats) "") model.feats }

                Nothing ->
                    model

        SetNote index note ->
            { model | feats = updateArrayAt index (setNoteOnSavedFeat note) model.feats }

        ToggleColumn col checked ->
            let
                newColumns =
                    if checked then
                        col :: model.columns

                    else
                        List.filter ((/=) col) model.columns
            in
            { model | columns = newColumns }

        SetEquipment me ->
            case me of
                Just e ->
                    { model | equipment = e }

                Nothing ->
                    model

        SetTableAccordion state ->
            { model | tableAccordionState = state }
    , Cmd.none
    )


columnToToggle : Model -> Column -> Html Msg
columnToToggle model col =
    Checkbox.checkbox
        [ col
            |> columnToToggleLabel
            |> Checkbox.id
        , Checkbox.onCheck <|
            ToggleColumn col
        , Checkbox.checked <| List.member col model.columns
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
                            [ Option Male "man" "M"
                            , Option Female "woman" "F"
                            , Option GNC "lifter" "GNC"
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
                            [ Option Total "totalled" "T"
                            , Option Squat "squatted" "S"
                            , Option Bench "benched" "B"
                            , Option Deadlift "deadlifted" "D"
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


view : Model -> Html Msg
view model =
    div []
        [ Grid.container []
            [ h1 [] [ text "Every Score Calculator" ]
            , lifterForm model
            , h2 [] [ text "Current Score" ]
            , Grid.row []
                [ Grid.col [ Col.xs12 ]
                    [ case model |> modelToFeat of
                        Just feat ->
                            featToTable initColumns feat

                        Nothing ->
                            Alert.simpleInfo [] [ text "Enter data to see all scores" ]
                    ]
                ]
            , Form.form []
                [ h2 []
                    [ text "Display columns" ]
                , Accordion.config SetTableAccordion
                    |> Accordion.withAnimation
                    |> Accordion.cards
                        [ Accordion.card
                            { id = "table-column-toggles"
                            , options = []
                            , header =
                                Accordion.toggle [] [ text "Table Options" ]
                                    |> Accordion.headerH2 []
                                    |> Accordion.prependHeader
                                        [ span
                                            [ (if Accordion.isOpen "table-column-toggles" model.tableAccordionState then
                                                "fa fa-chevron-down"

                                               else
                                                "fa fa-chevron-up"
                                              )
                                                |> class
                                            ]
                                            []
                                        ]
                            , blocks =
                                [ Accordion.block []
                                    [ Block.titleH4 [] [ text "Toggle Columns" ]
                                    , Block.text []
                                        [ Form.row
                                            []
                                            (initColumns
                                                |> List.map
                                                    (columnToToggle model
                                                        >> List.singleton
                                                        >> Form.col [ Col.xs6, Col.sm4, Col.md3, Col.lg3 ]
                                                    )
                                            )
                                        ]
                                    ]
                                ]
                            }
                        ]
                    |> Accordion.view model.tableAccordionState
                ]
            ]
        , Grid.containerFluid []
            [ Grid.row []
                [ Grid.col [ Col.sm12 ]
                    [ model.feats |> savedFeatsToTable (filterListByList model.columns initColumns) ]
                ]
            ]
        ]


savedFeatsToTable : List Column -> Array SavedFeat -> Html Msg
savedFeatsToTable cols =
    Array.indexedMap (savedFeatToRow cols)
        >> Array.toList
        >> rowsToHeadedTable
            ("Index"
                :: "Note"
                :: List.map
                    columnToToggleLabel
                    cols
            )


savedFeatToRow : List Column -> Int -> SavedFeat -> Table.Row Msg
savedFeatToRow cols index savedFeat =
    [ [ .index >> String.fromInt >> text
      , .note >> (\v -> Input.text [ Input.placeholder "Note", Input.value v, Input.onInput (SetNote index) ])
      ]
        |> List.map (thrush savedFeat)
    , (savedFeat.feat |> featToRecord |> thrush |> List.map) (List.map columnToRecordToText cols)
    ]
        |> List.concat
        |> htmlsToRow


featToTable : List Column -> Feat -> Html Msg
featToTable cols feat =
    (feat |> featToRecord |> thrush |> List.map) (List.map columnToRecordToText cols)
        |> List.map2
            (\label value ->
                Table.tr []
                    [ label |> columnToToggleLabel |> text |> List.singleton |> Table.td []
                    , value |> List.singleton |> Table.td []
                    ]
            )
            cols
        |> rowsToHeadedTable [ "Label", "Value" ]
