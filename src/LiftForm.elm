module LiftForm exposing
    ( State
    , init
    , toFeat
    , view
    )

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Form.Select as Select
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Dropdowns exposing (Option, typedSelect)
import Feat exposing (Equipment(..), Feat, Gender(..), Lift(..), MassUnit(..))
import Html as H exposing (Html)
import Html.Attributes as HA
import Library exposing (isStringPositiveFloat)



-- Model


type alias State =
    { liftedMass : String
    , liftedUnit : Feat.MassUnit
    , bodyMass : String
    , bodyUnit : Feat.MassUnit
    , gender : Gender
    , lift : Lift
    , age : String
    , equipment : Equipment
    , note : String
    }


toFeat : State -> Maybe Feat
toFeat state =
    case ( String.toFloat state.bodyMass, String.toFloat state.liftedMass ) of
        ( Just bodyMass, Just liftedMass ) ->
            Just
                { bodyKilos = Feat.massToKilos state.bodyUnit bodyMass
                , bodyPounds = Feat.massToPounds state.bodyUnit bodyMass
                , liftedKilos = Feat.massToKilos state.liftedUnit liftedMass
                , liftedPounds = Feat.massToPounds state.liftedUnit liftedMass
                , gender = state.gender
                , lift = state.lift
                , age = String.toFloat state.age
                , equipment = state.equipment
                , note = state.note
                }

        ( _, _ ) ->
            Nothing



-- Init


init : State
init =
    { liftedMass = ""
    , liftedUnit = Feat.KG
    , bodyMass = ""
    , bodyUnit = Feat.KG
    , gender = GNC
    , lift = Total
    , age = ""
    , equipment = Raw
    , note = ""
    }



--Update


updateEquipment : State -> (State -> msg) -> Maybe Equipment -> msg
updateEquipment state updateMsg maybeEquipment =
    case maybeEquipment of
        Just equipment ->
            updateMsg { state | equipment = equipment }

        Nothing ->
            updateMsg state


updateGender : State -> (State -> msg) -> Maybe Gender -> msg
updateGender state updateMsg maybeGender =
    case maybeGender of
        Just gender ->
            updateMsg { state | gender = gender }

        Nothing ->
            updateMsg state


updateLift : State -> (State -> msg) -> Maybe Lift -> msg
updateLift state updateMsg maybeLift =
    case maybeLift of
        Just lift ->
            updateMsg { state | lift = lift }

        Nothing ->
            updateMsg state


type Field
    = BodyMass
    | LiftedMass
    | Age
    | BodyUnit
    | LiftedUnit


updateNumeric : State -> (State -> msg) -> Field -> String -> msg
updateNumeric state updateMsg field val =
    (if isStringPositiveFloat val then
        case field of
            BodyMass ->
                { state | bodyMass = val }

            LiftedMass ->
                { state | liftedMass = val }

            Age ->
                { state | age = val }

            _ ->
                state

     else
        state
    )
        |> updateMsg


updateUnit : State -> (State -> msg) -> Field -> Feat.MassUnit -> msg
updateUnit state updateMsg field unit =
    (case field of
        BodyUnit ->
            { state | bodyUnit = unit }

        LiftedUnit ->
            { state | liftedUnit = unit }

        _ ->
            state
    )
        |> updateMsg



-- View


view : State -> (State -> msg) -> msg -> Html msg
view state updateMsg saveMsg =
    Form.form []
        [ H.h2 [] [ H.text "Lift input" ]
        , topRow state updateMsg
        , middleRow state updateMsg
        , thirdRow state updateMsg
        , bottomRow state updateMsg saveMsg
        ]


topRow : State -> (State -> msg) -> Html msg
topRow state updateMsg =
    Form.row [ Row.attrs [ HA.class " mb-0" ] ]
        [ Form.col [ Col.xs12, Col.md6 ]
            [ Form.row []
                [ Form.colLabel [ Col.xs4, Col.sm3 ] [ H.text "Gender" ]
                , Form.col [ Col.xs8, Col.sm9 ]
                    [ typedSelect [ Select.small ]
                        [ Option Male "Male" "M"
                        , Option Female "Female" "F"
                        , Option GNC "—" "GNC"
                        ]
                        state.gender
                      <|
                        updateGender state updateMsg
                    ]
                ]
            ]
        , Form.col [ Col.xs12, Col.md6 ]
            [ Form.row []
                [ Form.colLabel [ Col.xs4, Col.sm3 ] [ H.text "Event" ]
                , Form.col [ Col.xs8, Col.sm9 ]
                    [ typedSelect [ Select.small ]
                        [ Option Total "Total" "T"
                        , Option Squat "Squat" "S"
                        , Option Bench "Bench" "B"
                        , Option Deadlift "Deadlift" "D"
                        ]
                        state.lift
                      <|
                        updateLift state updateMsg
                    ]
                ]
            ]
        ]


middleRow : State -> (State -> msg) -> Html msg
middleRow state updateMsg =
    let
        updateNumeric_ =
            updateNumeric state updateMsg

        updateUnit_ =
            updateUnit state updateMsg
    in
    Form.row [ Row.attrs [ HA.class " mb-0" ] ]
        [ massInput
            { title = "Lifted weight"
            , numeric = state.liftedMass
            , updateNumeric = updateNumeric_ LiftedMass
            , unit = state.liftedUnit
            , updateUnit = updateUnit_ LiftedUnit
            }
        , massInput
            { title = "Bodyweight"
            , numeric = state.bodyMass
            , updateNumeric = updateNumeric_ BodyMass
            , unit = state.bodyUnit
            , updateUnit = updateUnit_ BodyUnit
            }
        ]


massInput :
    { title : String
    , numeric : String
    , updateNumeric : String -> msg
    , unit : Feat.MassUnit
    , updateUnit : Feat.MassUnit -> msg
    }
    -> Form.Col msg
massInput input =
    Form.col [ Col.xs12, Col.md6 ]
        [ Form.row []
            [ Form.colLabel [ Col.xs4, Col.sm3 ] [ H.text input.title ]
            , Form.col [ Col.xs8, Col.sm9 ]
                [ input.updateNumeric
                    |> numericInputOpts
                        input.numeric
                    |> InputGroup.text
                    |> InputGroup.config
                    |> InputGroup.successors
                        [ InputGroup.button
                            [ Button.outlineSecondary
                            , Feat.toggleMassUnit input.unit
                                |> input.updateUnit
                                |> Button.onClick
                            ]
                            [ H.text <|
                                case input.unit of
                                    KG ->
                                        "Kilos"

                                    LBM ->
                                        "Pounds"
                            ]
                        ]
                    |> InputGroup.small
                    |> InputGroup.view
                ]
            ]
        ]


numericInputOpts : String -> (String -> msg) -> List (Input.Option msg)
numericInputOpts value toMsg =
    [ Input.placeholder "0"
    , Input.onInput toMsg
    , Input.attrs [ HA.pattern "\\d+(\\.\\d+)?", HA.attribute "inputmode" "decimal" ]
    , Input.value value
    , Input.small
    ]


thirdRow : State -> (State -> msg) -> Html msg
thirdRow state updateMsg =
    Form.row [ Row.attrs [ HA.class " mb-0" ] ]
        [ Form.col [ Col.xs12, Col.md6 ]
            [ Form.row []
                [ Form.colLabel [ Col.xs4, Col.sm3 ] [ H.text "Note" ]
                , Form.col [ Col.xs8, Col.sm9 ]
                    [ Input.text
                        [ Input.onInput (updateMsg << (\s -> { state | note = s }))
                        ]
                    ]
                ]
            ]
        ]


bottomRow : State -> (State -> msg) -> msg -> Html msg
bottomRow state updateMsg saveMsg =
    Form.row [ Row.attrs [ HA.class " mb-0" ] ]
        [ Form.col [ Col.xs12, Col.md6 ]
            [ Form.row []
                [ Form.colLabel [ Col.xs4, Col.sm3 ] [ H.text "Equipment" ]
                , updateEquipment state updateMsg
                    |> typedSelect [ Select.small ]
                        [ Option Raw "raw" "R"
                        , Option SinglePly "single ply" "SP"
                        ]
                        state.equipment
                    |> List.singleton
                    |> Form.col [ Col.xs8, Col.sm9 ]
                ]
            ]
        , Form.col [ Col.xs12, Col.md6 ]
            [ Form.row []
                [ Form.col [ Col.xs7, Col.sm7 ]
                    [ Form.row []
                        [ Form.colLabel [ Col.xs7, Col.sm5 ] [ H.text "Age" ]
                        , updateNumeric state updateMsg Age
                            |> numericInputOpts
                                state.age
                            |> Input.text
                            |> List.singleton
                            |> Form.col [ Col.xs5, Col.sm7 ]
                        ]
                    ]
                , ((case toFeat state of
                        Just _ ->
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
                    [ H.text "Add to Table"
                    ]
                    |> List.singleton
                    |> Form.col [ Col.xs5, Col.sm5 ]
                ]
            ]
        ]
