module LiftForm exposing
    ( Intent(..)
    , State
    , fieldsToFeat
    , init
    , popState
    , pushSavedFeat
    , toFeat
    , toSavedFeat
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
import SavedFeat exposing (SavedFeat)



-- Model


type State
    = UpdatingState
        { pushedState : State
        , key : Int
        , fields : FieldsState
        , backup : SavedFeat
        }
    | NormalState FieldsState


type alias FieldsState =
    { liftedMass : String
    , liftedUnit : Mass.MassUnit
    , bodyMass : String
    , bodyUnit : Mass.MassUnit
    , gender : Gender
    , lift : Lift
    , age : String
    , equipment : Equipment
    , note : String
    }


type Result
    = UpdateSavedFeat SavedFeat State
    | CannotUpdateSavedFeat SavedFeat
    | NewFeat Feat
    | Incomplete


type Intent
    = Update State SavedFeat
    | Create State Feat
    | State State


toResult : State -> Result
toResult state =
    case ( toFeat state, state ) of
        ( Nothing, _ ) ->
            Incomplete

        ( Just feat, NormalState _ ) ->
            NewFeat feat

        ( Just feat, UpdatingState s ) ->
            let
                savedFeat =
                    SavedFeat.SavedFeat s.key feat
            in
            if s.backup == savedFeat then
                CannotUpdateSavedFeat s.backup

            else
                UpdateSavedFeat savedFeat <|
                    UpdatingState
                        { s | fields = fromFeat s.backup.feat }


toFieldsState : State -> FieldsState
toFieldsState state =
    case state of
        UpdatingState s ->
            s.fields

        NormalState s ->
            s


toSavedFeat : State -> Int -> Maybe SavedFeat
toSavedFeat state key =
    case state of
        UpdatingState s ->
            Maybe.map (SavedFeat.SavedFeat s.key) (fieldsToFeat s.fields)

        NormalState s ->
            Maybe.map (SavedFeat.SavedFeat key) (fieldsToFeat s)


pushSavedFeat : State -> SavedFeat.SavedFeat -> State
pushSavedFeat oldState savedFeat =
    UpdatingState
        { pushedState = oldState
        , key = savedFeat.key
        , fields = fromFeat savedFeat.feat
        , backup = savedFeat
        }


popState : State -> State
popState state =
    case state of
        UpdatingState s ->
            s.pushedState

        NormalState _ ->
            state


toFeat : State -> Maybe Feat
toFeat =
    toFieldsState >> fieldsToFeat


fieldsToFeat : FieldsState -> Maybe Feat
fieldsToFeat state =
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


fromFeat : Feat -> FieldsState
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
    }


updateFieldsState : State -> FieldsState -> State
updateFieldsState state fields =
    case state of
        UpdatingState s ->
            UpdatingState { s | fields = fields }

        NormalState _ ->
            NormalState fields



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
    }
        |> NormalState



--Update


updateEquipment : State -> (State -> msg) -> Maybe Equipment -> msg
updateEquipment state updateMsg maybeEquipment =
    let
        fields =
            toFieldsState state
    in
    (case maybeEquipment of
        Just equipment ->
            { fields | equipment = equipment }
                |> updateFieldsState state

        Nothing ->
            state
    )
        |> updateMsg


updateGender : State -> (State -> msg) -> Maybe Gender -> msg
updateGender state updateMsg maybeGender =
    let
        fields =
            toFieldsState state
    in
    (case maybeGender of
        Just gender ->
            { fields | gender = gender }
                |> updateFieldsState state

        Nothing ->
            state
    )
        |> updateMsg


updateLift : State -> (State -> msg) -> Maybe Lift -> msg
updateLift state updateMsg maybeLift =
    let
        fields =
            toFieldsState state
    in
    (case maybeLift of
        Just lift ->
            { fields | lift = lift }
                |> updateFieldsState state

        Nothing ->
            state
    )
        |> updateMsg


updateNote : State -> (State -> msg) -> String -> msg
updateNote state updateMsg note =
    let
        fields =
            toFieldsState state
    in
    { fields | note = note }
        |> updateFieldsState state
        |> updateMsg


type Field
    = BodyMass
    | LiftedMass
    | Age
    | BodyUnit
    | LiftedUnit


updateNumeric : State -> (State -> msg) -> Field -> String -> msg
updateNumeric state updateMsg field val =
    let
        fields =
            toFieldsState state
    in
    (if isStringPositiveFloat val then
        case field of
            BodyMass ->
                { fields | bodyMass = val }
                    |> updateFieldsState state

            LiftedMass ->
                { fields | liftedMass = val }
                    |> updateFieldsState state

            Age ->
                { fields | age = val }
                    |> updateFieldsState state

            _ ->
                state

     else
        state
    )
        |> updateMsg


updateUnit : State -> (State -> msg) -> Field -> Mass.MassUnit -> msg
updateUnit state updateMsg field unit =
    let
        fields =
            toFieldsState state
    in
    (case field of
        BodyUnit ->
            { fields | bodyUnit = unit }
                |> updateFieldsState state

        LiftedUnit ->
            { fields | liftedUnit = unit }
                |> updateFieldsState state

        _ ->
            state
    )
        |> updateMsg



-- View


view : State -> (Intent -> msg) -> Html msg
view state msg =
    let
        stateMsg =
            State >> msg
    in
    Form.form []
        [ H.h2 [] [ H.text "Lift input" ]
        , topRow state stateMsg
        , middleRow state stateMsg
        , thirdRow state stateMsg
        , bottomRow state msg
        ]


topRow : State -> (State -> msg) -> Html msg
topRow state updateMsg =
    let
        fields =
            toFieldsState state
    in
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
                        fields.gender
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
                        fields.lift
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

        fields =
            toFieldsState state
    in
    Form.row [ Row.attrs [ HA.class " mb-0" ] ]
        [ massInput
            { title = "Lifted wt."
            , numeric = fields.liftedMass
            , updateNumeric = updateNumeric_ LiftedMass
            , unit = fields.liftedUnit
            , updateUnit = updateUnit_ LiftedUnit
            }
        , massInput
            { title = "Bodywt."
            , numeric = fields.bodyMass
            , updateNumeric = updateNumeric_ BodyMass
            , unit = fields.bodyUnit
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
            [ Form.colLabel
                [ Col.xs4
                , Col.sm3
                , Col.attrs [ HA.style "white-space" "nowrap" ]
                ]
                [ H.text input.title ]
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
thirdRow state updateMsg =
    let
        fields =
            toFieldsState state
    in
    Form.row [ Row.attrs [ HA.class " mb-0" ] ]
        [ Form.col [ Col.xs12, Col.md6 ]
            [ Form.row []
                [ Form.colLabel [ Col.xs4, Col.sm3 ] [ H.text "Note" ]
                , Form.col [ Col.xs8, Col.sm9 ]
                    [ Input.text
                        [ Input.onInput <| updateNote state updateMsg
                        , Input.value fields.note
                        , Input.small
                        ]
                    ]
                ]
            ]
        , Form.col [ Col.xs12, Col.md6 ]
            [ Form.row []
                [ Form.colLabel [ Col.xs4, Col.sm3 ] [ H.text "Equipment" ]
                , updateEquipment state updateMsg
                    |> typedSelect [ Select.small ]
                        [ Option Raw "raw" "R"
                        , Option SinglePly "single ply" "SP"
                        ]
                        fields.equipment
                    |> List.singleton
                    |> Form.col [ Col.xs8, Col.sm9 ]
                ]
            ]
        ]


bottomRow : State -> (Intent -> msg) -> Html msg
bottomRow state msg =
    let
        fields =
            toFieldsState state

        stateMsg =
            State >> msg
    in
    Form.row []
        [ Form.col [ Col.xs12, Col.md6 ]
            [ Form.row []
                [ Form.colLabel [ Col.xs4, Col.sm3 ] [ H.text "Age" ]
                , Form.col [ Col.xs8, Col.sm9, Col.attrs [ HA.class "testing" ] ]
                    [ Form.row [ Row.attrs [ HA.class "mb-0" ] ]
                        [ updateNumeric state stateMsg Age
                            |> numericInputOpts
                                fields.age
                            |> Input.text
                            |> List.singleton
                            |> Form.col [ Col.xs12, Col.sm12 ]
                        ]
                    ]
                ]
            ]
        , (case toResult state of
            CannotUpdateSavedFeat savedFeat ->
                [ Button.button
                    [ Button.block
                    , Button.small
                    , Button.outlineDanger
                    , Update
                        (popState state)
                        savedFeat
                        |> msg
                        |> Button.onClick
                    ]
                    [ H.text "Cancel update"
                    ]
                ]

            UpdateSavedFeat savedFeat backupState ->
                [ Button.button
                    [ Button.block
                    , Button.small
                    , Button.outlineDanger
                    , backupState
                        |> stateMsg
                        |> Button.onClick
                    ]
                    [ H.text "Reset Changes"
                    ]
                , Button.button
                    [ Button.block
                    , Button.small
                    , Button.success
                    , Update (popState state) savedFeat
                        |> msg
                        |> Button.onClick
                    ]
                    [ H.text "Update"
                    ]
                ]

            NewFeat feat ->
                Button.button
                    [ Button.block
                    , Button.small
                    , Button.success
                    , Create state feat
                        |> msg
                        |> Button.onClick
                    ]
                    [ H.text "Add to table"
                    ]
                    |> List.singleton

            Incomplete ->
                Button.button
                    [ Button.block
                    , Button.small
                    , Button.disabled True
                    , Button.small
                    , Button.secondary
                    ]
                    [ H.text "More Info Needed to Save"
                    ]
                    |> List.singleton
          )
            |> Form.col [ Col.xs12, Col.md6 ]
        ]
