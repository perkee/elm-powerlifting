module View.LiftForm exposing (view)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Form.Select as Select
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Data.LiftForm exposing (State, toFeat)
import Dropdowns exposing (Option, typedSelect)
import Feat exposing (Equipment(..), Gender(..), Lift(..), MassUnit(..))
import Html exposing (Html, h2, text)
import Html.Attributes exposing (attribute, class, pattern)
import Html.Events exposing (onClick, onInput)
import View.UnitDropdown as UnitDropdown


view : State -> (State -> msg) -> msg -> Html msg
view state updateMsg saveMsg =
    Form.form []
        [ h2 [] [ text "Lift input" ]
        , topRow state updateMsg
        , middleRow state updateMsg
        , bottomRow state updateMsg saveMsg
        ]


topRow : State -> (State -> msg) -> Html msg
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
                        (updateMsg
                            << (\mg ->
                                    case mg of
                                        Just g ->
                                            { state | gender = g }

                                        Nothing ->
                                            state
                               )
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


middleRow : State -> (State -> msg) -> Html msg
middleRow state updateMsg =
    Form.row [ Row.attrs [ class " mb-0" ] ]
        [ Form.col [ Col.xs12, Col.md6 ]
            [ Form.row []
                [ Form.colLabel [ Col.xs4, Col.sm3 ] [ text "Lifted weight" ]
                , Form.col [ Col.xs8, Col.sm9 ]
                    [ InputGroup.config
                        (InputGroup.number
                            [ Input.placeholder "0"
                            , Input.onInput (\newMass -> updateMsg { state | liftedMass = newMass })
                            , Input.attrs [ pattern "\\d+(\\.\\d+)?", attribute "inputmode" "decimal" ]
                            ]
                        )
                        |> InputGroup.successors
                            [ UnitDropdown.view
                                state.liftedUnit
                                ((\s -> { state | liftedUnit = s }) >> updateMsg)
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
                            , Input.onInput <| (\newMass -> { state | bodyMass = newMass }) >> updateMsg
                            , Input.attrs [ pattern "\\d+(\\.\\d+)?", attribute "inputmode" "decimal" ]
                            ]
                        )
                        |> InputGroup.successors
                            [ UnitDropdown.view
                                state.bodyUnit
                                ((\s -> { state | bodyUnit = s }) >> updateMsg)
                            ]
                        |> InputGroup.small
                        |> InputGroup.view
                    ]
                ]
            ]
        ]


bottomRow : State -> (State -> msg) -> msg -> Html msg
bottomRow state updateMsg saveMsg =
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
                                , Input.onInput (\newAge -> updateMsg { state | age = newAge })
                                , Input.attrs [ pattern "\\d+(\\.\\d+)?", attribute "inputmode" "decimal" ]
                                , Input.value state.age
                                , Input.small
                                ]
                            ]
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
                    [ text "Add to Table"
                    ]
                    |> List.singleton
                    |> Form.col [ Col.xs5, Col.sm5 ]
                ]
            ]
        ]
