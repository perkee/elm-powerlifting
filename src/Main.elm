module Main exposing (main)

-- (Html, button, div, text, input, option, select)
-- imports used

import Array exposing (Array)
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
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
        , featToString
        , recordToCells
        , recordToPara
        , recordToTable
        )


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Dropdown.subscriptions model.liftedUnitState SetLiftedUnitState ]


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
    | SetLiftedUnit (Maybe MassUnit)
    | SetLiftedUnitState Dropdown.State
    | SetBodyMass String
    | SetBodyUnit (Maybe MassUnit)
    | SetBodyUnitState Dropdown.State
    | SetGender (Maybe Gender)
    | SetLift (Maybe Lift)
    | SetAge String
    | SaveFeat
    | SetNote Int String
    | ToggleColumn Column Bool
    | SetEquipment (Maybe Equipment)


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

        SetLiftedUnit mu ->
            case mu of
                Just u ->
                    { model | liftedUnit = u }

                Nothing ->
                    model

        SetLiftedUnitState state ->
            { model | liftedUnitState = state }

        SetBodyMass s ->
            { model | bodyMass = ffParse s }

        SetBodyUnit mu ->
            case mu of
                Just u ->
                    { model | bodyUnit = u }

                Nothing ->
                    model

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


modelToScoresDom : Model -> Html Msg
modelToScoresDom m =
    div []
        [ m |> modelToRecord |> recordToTable
        , m.feats |> savedFeatsToTable (filterListByList m.columns initColumns)
        ]


unitSelect : MassUnit -> (Maybe MassUnit -> Msg) -> Html Msg
unitSelect =
    typedSelect []
        [ Option KG "kilos" "KG"
        , Option LBM "pounds" "LBM"
        ]


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
        ]
        [ (if canSave then
            "Record"

           else
            "Type first!"
          )
            |> text
        ]


fakeRow : List any -> List other -> List other
fakeRow options children =
    children


unitDropDown : Dropdown.State -> (Dropdown.State -> Msg) -> MassUnit -> (Maybe MassUnit -> Msg) -> InputGroup.Addon Msg
unitDropDown state stateMsg massUnit unitMsg =
    InputGroup.dropdown
        state
        { options = []
        , toggleMsg = stateMsg
        , toggleButton =
            Dropdown.toggle [ Button.primary ]
                [ (case massUnit of
                    KG ->
                        "Kilos"

                    LBM ->
                        "Pounds"
                  )
                    |> text
                ]
        , items =
            [ Dropdown.buttonItem [ KG |> Just |> unitMsg |> onClick ] [ text "Kilos" ]
            , Dropdown.buttonItem [ LBM |> Just |> unitMsg |> onClick ] [ text "Pounds" ]
            ]
        }


lifterForm : Model -> Html Msg
lifterForm model =
    Form.form []
        [ h2 [] [ text "Lift input" ]
        , Form.row []
            [ Form.col [ Col.xs12, Col.md6 ]
                [ Form.row []
                    [ Form.colLabel [ Col.xs4, Col.sm3 ] [ text "Gender" ]
                    , Form.col [ Col.xs8, Col.sm9 ]
                        [ typedSelect []
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
                        [ typedSelect []
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
        , Form.row []
            [ Form.col [ Col.xs12, Col.md6 ]
                [ Form.row []
                    [ Form.colLabel [ Col.xs4, Col.sm3 ] [ text "Lifted weight" ]
                    , Form.col [ Col.xs8, Col.sm9 ]
                        [ Form.row []
                            [ Form.col [ Col.xs12 ]
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
                                    |> InputGroup.view
                                ]
                            ]
                        ]
                    ]
                ]
            , Form.col [ Col.xs12, Col.md6 ]
                [ Form.row []
                    [ Form.colLabel [ Col.xs4, Col.sm3 ] [ text "Bodyweight" ]
                    , Form.col [ Col.xs8, Col.sm9 ]
                        [ Form.row []
                            [ Form.col [ Col.xs12 ]
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
                                    |> InputGroup.view
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , Form.row []
            [ Form.col [ Col.xs12, Col.md6 ]
                [ Form.row []
                    [ Form.colLabel [ Col.xs4, Col.sm3 ] [ text "Equipment" ]
                    , Form.col [ Col.xs8, Col.sm9 ]
                        [ typedSelect [ Select.disabled True ]
                            [ Option Raw "raw" "R"
                            , Option SinglePly "single ply" "SP"
                            ]
                            model.equipment
                            SetEquipment
                        ]
                    ]
                ]
            , Form.col [ Col.xs12, Col.md6 ]
                [ Form.row []
                    [ Form.col [ Col.xs12, Col.sm7 ]
                        [ Form.row []
                            [ Form.colLabel [ Col.xs4, Col.sm5 ] [ text "Age" ]
                            , Form.col [ Col.xs8, Col.sm7 ]
                                [ viewFloatInput "ageInput" model.bodyMass.input SetAge
                                ]
                            ]
                        ]
                    , Form.col [ Col.xs12, Col.sm5 ]
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
            [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
            , h1 [] [ text "Every Score Calculator" ]
            , lifterForm model
            , Grid.row []
                [ Grid.col [ Col.sm12 ]
                    [ model |> modelToRecord |> recordToTable ]
                ]
            , Form.form []
                [ h2 []
                    [ text "Display columns" ]
                , Form.row
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
        , Grid.containerFluid []
            [ Grid.row []
                [ Grid.col [ Col.sm12 ]
                    [ model.feats |> savedFeatsToTable (filterListByList model.columns initColumns) ]
                ]
            ]
        ]


viewFloatInput : String -> String -> (String -> msg) -> Html msg
viewFloatInput id v toMsg =
    Input.number
        [ Input.id id
        , Input.placeholder "0"
        , Input.onInput toMsg
        , Input.attrs [ pattern "\\d+(\\.\\d+)?", attribute "inputmode" "decimal" ]
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
