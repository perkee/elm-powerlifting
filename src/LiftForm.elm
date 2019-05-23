module LiftForm exposing (FormState, formStateToFeat, formStateToSubs, initFormState, lifterForm)

import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Form.Select as Select
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Dropdowns exposing (Option, typedSelect)
import Feat exposing (Equipment(..), Feat, Gender(..), Lift(..), MassUnit(..), massToKilos, massToPounds)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onCheck, onClick, onInput, targetValue)
import Platform.Sub
import Renderer exposing (FloatField, initFloatField, stringToFloatField)


type alias FormState =
    { liftedMass : FloatField
    , liftedUnit : MassUnit
    , liftedUnitState : Dropdown.State
    , bodyMass : FloatField
    , bodyUnit : MassUnit
    , bodyUnitState : Dropdown.State
    , gender : Gender
    , lift : Lift
    , age : FloatField
    , equipment : Equipment
    }


formStateToSubs : FormState -> (FormState -> msg) -> Platform.Sub.Sub msg
formStateToSubs state updateMsg =
    Sub.batch
        [ Dropdown.subscriptions state.liftedUnitState (\newDrop -> updateMsg { state | liftedUnitState = newDrop })
        , Dropdown.subscriptions state.bodyUnitState (\newDrop -> updateMsg { state | bodyUnitState = newDrop })
        ]


initFormState : FormState
initFormState =
    { liftedMass = initFloatField
    , liftedUnit = KG
    , liftedUnitState = Dropdown.initialState
    , bodyMass = initFloatField
    , bodyUnit = KG
    , bodyUnitState = Dropdown.initialState
    , gender = GNC
    , lift = Total
    , age = initFloatField
    , equipment = Raw
    }


formStateToFeat : FormState -> Maybe Feat
formStateToFeat state =
    case ( state.bodyMass.value, state.liftedMass.value ) of
        ( Just bodyMass, Just liftedMass ) ->
            Just
                { bodyKilos = massToKilos state.bodyUnit bodyMass
                , bodyPounds = massToPounds state.bodyUnit bodyMass
                , liftedKilos = massToKilos state.liftedUnit liftedMass
                , liftedPounds = massToPounds state.liftedUnit liftedMass
                , gender = state.gender
                , lift = state.lift
                , age = state.age.value
                , equipment = state.equipment
                }

        ( _, _ ) ->
            Nothing


unitDropDown : Dropdown.State -> (Dropdown.State -> msg) -> MassUnit -> (MassUnit -> msg) -> InputGroup.Addon msg
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


lifterForm : FormState -> Maybe Feat -> (FormState -> msg) -> msg -> Html msg
lifterForm state currentFeat updateMsg saveMsg =
    Form.form []
        [ h2 [] [ text "Lift input" ]
        , topRow state updateMsg
        , middleRow state updateMsg
        , bottomRow state currentFeat updateMsg saveMsg
        ]


topRow : FormState -> (FormState -> msg) -> Html msg
topRow state updateMsg =
    Form.row [ Row.attrs [ class " mb-0" ] ]
        [ Form.col [ Col.xs12, Col.md6 ]
            [ Form.row []
                [ Form.colLabel [ Col.xs4, Col.sm3 ] [ text "Gender" ]
                , Form.col [ Col.xs8, Col.sm9 ]
                    [ typedSelect [ Select.small ]
                        [ Option Male "Male" "M"
                        , Option Female "Female" "F"
                        , Option GNC "—" "GNC"
                        ]
                        state.gender
                        (\mg ->
                            case mg of
                                Just g ->
                                    updateMsg { state | gender = g }

                                Nothing ->
                                    updateMsg state
                        )
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
                        state.lift
                        (\ml ->
                            case ml of
                                Just l ->
                                    updateMsg { state | lift = l }

                                Nothing ->
                                    updateMsg state
                        )
                    ]
                ]
            ]
        ]


middleRow : FormState -> (FormState -> msg) -> Html msg
middleRow state updateMsg =
    Form.row [ Row.attrs [ class " mb-0" ] ]
        [ Form.col [ Col.xs12, Col.md6 ]
            [ Form.row []
                [ Form.colLabel [ Col.xs4, Col.sm3 ] [ text "Lifted weight" ]
                , Form.col [ Col.xs8, Col.sm9 ]
                    [ InputGroup.config
                        (InputGroup.number
                            [ Input.placeholder "0"
                            , Input.onInput (\newMass -> updateMsg { state | liftedMass = stringToFloatField newMass })
                            , Input.attrs [ pattern "\\d+(\\.\\d+)?", attribute "inputmode" "decimal" ]
                            ]
                        )
                        |> InputGroup.successors
                            [ unitDropDown
                                state.liftedUnitState
                                (\newDrop -> updateMsg { state | liftedUnitState = newDrop })
                                state.liftedUnit
                                (\newUnit -> updateMsg { state | liftedUnit = newUnit })
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
                            , Input.onInput (\newMass -> updateMsg { state | bodyMass = stringToFloatField newMass })
                            , Input.attrs [ pattern "\\d+(\\.\\d+)?", attribute "inputmode" "decimal" ]
                            ]
                        )
                        |> InputGroup.successors
                            [ unitDropDown
                                state.bodyUnitState
                                (\newDrop -> updateMsg { state | bodyUnitState = newDrop })
                                state.bodyUnit
                                (\newUnit -> updateMsg { state | bodyUnit = newUnit })
                            ]
                        |> InputGroup.small
                        |> InputGroup.view
                    ]
                ]
            ]
        ]


bottomRow : FormState -> Maybe Feat -> (FormState -> msg) -> msg -> Html msg
bottomRow state currentFeat updateMsg saveMsg =
    Form.row [ Row.attrs [ class " mb-0" ] ]
        [ Form.col [ Col.xs12, Col.md6 ]
            [ Form.row []
                [ Form.colLabel [ Col.xs4, Col.sm3 ] [ text "Equipment" ]
                , Form.col [ Col.xs8, Col.sm9 ]
                    [ typedSelect [ Select.small ]
                        [ Option Raw "raw" "R"
                        , Option SinglePly "single ply" "SP"
                        ]
                        state.equipment
                        (\me ->
                            case me of
                                Just e ->
                                    updateMsg { state | equipment = e }

                                Nothing ->
                                    updateMsg state
                        )
                    ]
                ]
            ]
        , Form.col [ Col.xs12, Col.md6 ]
            [ Form.row []
                [ Form.col [ Col.xs7, Col.sm7 ]
                    [ Form.row []
                        [ Form.colLabel [ Col.xs7, Col.sm5 ] [ text "Age" ]
                        , Form.col [ Col.xs5, Col.sm7 ]
                            [ Input.number
                                [ Input.placeholder "0"
                                , Input.onInput (\newAge -> updateMsg { state | age = stringToFloatField newAge })
                                , Input.attrs [ pattern "\\d+(\\.\\d+)?", attribute "inputmode" "decimal" ]
                                , Input.value state.age.input
                                , Input.small
                                ]
                            ]
                        ]
                    ]
                , ((case currentFeat of
                        Just feat ->
                            [ Button.success
                            , Button.onClick saveMsg
                            ]

                        Nothing ->
                            [ Button.disabled True
                            , Button.small
                            , Button.secondary
                            ]
                   )
                    |> (++)
                        [ Button.block
                        , Button.small
                        ]
                    |> Button.button
                  )
                    [ text "Add to Table"
                    ]
                    |> List.singleton
                    |> Form.col [ Col.xs5, Col.sm5 ]
                ]
            ]
        ]
