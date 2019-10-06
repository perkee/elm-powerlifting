module LiftForm exposing
    ( State
    , init
    , popState
    , pushFeat
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
import Feat exposing (Equipment(..), Feat, Gender(..), Lift(..))
import Html as H exposing (Html)
import Html.Attributes as HA
import Library exposing (isStringPositiveFloat)
import Mass



-- Model


type State
    = State
        { liftedMass : String
        , liftedUnit : Mass.MassUnit
        , bodyMass : String
        , bodyUnit : Mass.MassUnit
        , gender : Gender
        , lift : Lift
        , age : String
        , equipment : Equipment
        , note : String
        , pushedState : Maybe State
        }


pushFeat : State -> Feat -> State
pushFeat oldState feat =
    pushState oldState <| fromFeat feat


pushState : State -> State -> State
pushState oldState (State newState) =
    State { newState | pushedState = Just oldState }


popState : State -> State
popState (State state) =
    case state.pushedState of
        Just oldState ->
            oldState

        Nothing ->
            State state


toFeat : State -> Maybe Feat
toFeat (State state) =
    case
        ( String.toFloat state.bodyMass
        , String.toFloat state.liftedMass
        )
    of
        ( Just bodyMass, Just liftedMass ) ->
            Just
                { bodyMass = Mass.fromUnitAndFloat state.bodyUnit bodyMass
                , liftedMass = Mass.fromUnitAndFloat state.liftedUnit liftedMass
                , gender = state.gender
                , lift = state.lift
                , age = String.toFloat state.age
                , equipment = state.equipment
                , note = state.note
                }

        ( _, _ ) ->
            Nothing


fromFeat : Feat -> State
fromFeat feat =
    let
        ( liftUnit, liftFloat ) =
            Mass.toUnitAndFloat feat.liftedMass

        ( bodyUnit, bodyFloat ) =
            Mass.toUnitAndFloat feat.bodyMass
    in
    { liftedMass = String.fromFloat liftFloat
    , liftedUnit = liftUnit
    , bodyMass = String.fromFloat bodyFloat
    , bodyUnit = bodyUnit
    , gender = feat.gender
    , lift = feat.lift
    , age =
        case feat.age of
            Just age ->
                String.fromFloat age

            Nothing ->
                ""
    , equipment = feat.equipment
    , note = feat.note
    , pushedState = Nothing
    }
        |> State



-- Init


init : State
init =
    { liftedMass = ""
    , liftedUnit = Mass.KG
    , bodyMass = ""
    , bodyUnit = Mass.KG
    , gender = GNC
    , lift = Total
    , age = ""
    , equipment = Raw
    , note = ""
    , pushedState = Nothing
    }
        |> State



--Update


updateEquipment : State -> (State -> msg) -> Maybe Equipment -> msg
updateEquipment (State state) updateMsg maybeEquipment =
    (case maybeEquipment of
        Just equipment ->
            { state | equipment = equipment }

        Nothing ->
            state
    )
        |> State
        |> updateMsg


updateGender : State -> (State -> msg) -> Maybe Gender -> msg
updateGender (State state) updateMsg maybeGender =
    (case maybeGender of
        Just gender ->
            { state | gender = gender }

        Nothing ->
            state
    )
        |> State
        |> updateMsg


updateLift : State -> (State -> msg) -> Maybe Lift -> msg
updateLift (State state) updateMsg maybeLift =
    (case maybeLift of
        Just lift ->
            { state | lift = lift }

        Nothing ->
            state
    )
        |> State
        |> updateMsg


updateNote : State -> (State -> msg) -> String -> msg
updateNote (State state) updateMsg note =
    { state | note = note }
        |> State
        |> updateMsg


type Field
    = BodyMass
    | LiftedMass
    | Age
    | BodyUnit
    | LiftedUnit


updateNumeric : State -> (State -> msg) -> Field -> String -> msg
updateNumeric (State state) updateMsg field val =
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
        |> State
        |> updateMsg


updateUnit : State -> (State -> msg) -> Field -> Mass.MassUnit -> msg
updateUnit (State state) updateMsg field unit =
    (case field of
        BodyUnit ->
            { state | bodyUnit = unit }

        LiftedUnit ->
            { state | liftedUnit = unit }

        _ ->
            state
    )
        |> State
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
topRow (State state) updateMsg =
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
                        updateGender (State state) updateMsg
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
                        updateLift (State state) updateMsg
                    ]
                ]
            ]
        ]


middleRow : State -> (State -> msg) -> Html msg
middleRow (State state) updateMsg =
    let
        updateNumeric_ =
            updateNumeric (State state) updateMsg

        updateUnit_ =
            updateUnit (State state) updateMsg
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
    , unit : Mass.MassUnit
    , updateUnit : Mass.MassUnit -> msg
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
                            , Mass.toggleMassUnit input.unit
                                |> input.updateUnit
                                |> Button.onClick
                            ]
                            [ H.text <|
                                case input.unit of
                                    Mass.KG ->
                                        "Kilos"

                                    Mass.LBM ->
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
thirdRow (State state) updateMsg =
    Form.row [ Row.attrs [ HA.class " mb-0" ] ]
        [ Form.col [ Col.xs12, Col.md6 ]
            [ Form.row []
                [ Form.colLabel [ Col.xs4, Col.sm3 ] [ H.text "Note" ]
                , Form.col [ Col.xs8, Col.sm9 ]
                    [ Input.text
                        [ Input.onInput <| updateNote (State state) updateMsg
                        , Input.value state.note
                        ]
                    ]
                ]
            ]
        ]


bottomRow : State -> (State -> msg) -> msg -> Html msg
bottomRow (State state) updateMsg saveMsg =
    Form.row [ Row.attrs [ HA.class " mb-0" ] ]
        [ Form.col [ Col.xs12, Col.md6 ]
            [ Form.row []
                [ Form.colLabel [ Col.xs4, Col.sm3 ] [ H.text "Equipment" ]
                , updateEquipment (State state) updateMsg
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
                        , updateNumeric (State state) updateMsg Age
                            |> numericInputOpts
                                state.age
                            |> Input.text
                            |> List.singleton
                            |> Form.col [ Col.xs5, Col.sm7 ]
                        ]
                    ]
                , ((case toFeat (State state) of
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
