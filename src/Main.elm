module Main exposing (main)

-- (Html, button, div, text, input, option, select)

import Array exposing (Array)
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
    Browser.sandbox { init = init, update = update, view = view }


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
    , bodyMass : FloatField
    , bodyUnit : MassUnit
    , gender : Gender
    , lift : Lift
    , age : FloatField
    , feats : Array SavedFeat
    , columns : List Column
    , equipment : Equipment
    }


init : Model
init =
    { liftedMass = initFloatField
    , liftedUnit = LBM
    , bodyMass = initFloatField
    , bodyUnit = LBM
    , gender = GNC
    , lift = Total
    , age = initFloatField
    , feats = Array.empty
    , columns = initColumns
    , equipment = Raw
    }


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
    | SetBodyMass String
    | SetBodyUnit MassUnit
    | SetGender Gender
    | SetLift Lift
    | SetAge String
    | SaveFeat
    | SetNote Int String
    | ToggleColumn Column Bool
    | SetEquipment Equipment


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


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetLiftedMass s ->
            { model | liftedMass = ffParse s }

        SetLiftedUnit u ->
            { model | liftedUnit = u }

        SetBodyMass s ->
            { model | bodyMass = ffParse s }

        SetBodyUnit u ->
            { model | bodyUnit = u }

        SetGender s ->
            { model | gender = s }

        SetLift l ->
            { model | lift = l }

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

        SetEquipment equipment ->
            { model | equipment = equipment }


columnToToggle : Model -> Column -> Html Msg
columnToToggle model col =
    label
        [ col |> columnToToggleLabel |> for
        ]
        [ col |> columnToToggleLabel |> text
        , input
            [ type_ "checkbox"
            , checked <| List.member col model.columns
            , onCheck <| ToggleColumn col
            ]
            []
        ]


modelToScoresDom : Model -> Html Msg
modelToScoresDom m =
    div []
        [ m |> modelToRecord |> recordToTable
        , m |> modelToRecord |> recordToPara
        , m.feats
            |> Array.map
                (.feat
                    >> featToString
                    >> text
                    >> List.singleton
                    >> li []
                )
            >> Array.toList
            |> ol []
        , m.feats |> savedFeatsToTable (filterListByList m.columns initColumns)
        ]


unitSelect : MassUnit -> (MassUnit -> Msg) -> Html Msg
unitSelect =
    typedSelect
        [ Option KG "kilos" "KG"
        , Option LBM "pounds" "LBM"
        ]


view : Model -> Html Msg
view model =
    div []
        [ label [ for "genderInput" ] [ text "A " ]
        , typedSelect
            [ Option Male "man" "M"
            , Option Female "woman" "F"
            , Option GNC "lifter" "GNC"
            ]
            model.gender
            SetGender
        , typedSelect
            [ Option Raw "raw" "R"
            , Option SinglePly "single ply" "SP"
            ]
            model.equipment
            SetEquipment
        , typedSelect
            [ Option Total "totalled" "T"
            , Option Squat "squatted" "S"
            , Option Bench "benched" "B"
            , Option Deadlift "deadlifted" "D"
            ]
            model.lift
            SetLift
        , viewFloatInput "liftedInput" model.liftedMass.input SetLiftedMass
        , unitSelect model.liftedUnit SetLiftedUnit
        , label [ for "bodyInput" ] [ text " weighing " ]
        , viewFloatInput "bodyInput" model.bodyMass.input SetBodyMass
        , unitSelect model.bodyUnit SetBodyUnit
        , label [ for "ageInput" ]
            [ text " at "
            , viewFloatInput "ageInput" model.bodyMass.input SetAge
            , text " years old."
            ]
        , button
            [ onClick SaveFeat
            , model |> modelToFeat |> canMakeFeat |> not |> disabled
            ]
            [ text "save" ]
        , div []
            (text
                "toggle columns"
                :: (initColumns
                        |> List.map (columnToToggle model)
                   )
            )
        , modelToScoresDom model
        ]


viewFloatInput : String -> String -> (String -> msg) -> Html msg
viewFloatInput id v toMsg =
    input
        [ Html.Attributes.id id
        , type_ "number"
        , placeholder "0"
        , onInput toMsg
        ]
        []


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


savedFeatToRow : List Column -> Int -> SavedFeat -> Html Msg
savedFeatToRow cols index savedFeat =
    [ [ .index >> String.fromInt >> text
      , .note >> (\v -> input [ type_ "text", placeholder "Note", value v, onInput (SetNote index) ] [])
      ]
        |> List.map (thrush savedFeat)
    , (savedFeat.feat |> featToRecord |> thrush |> List.map) (List.map columnToRecordToText cols)
    ]
        |> List.concat
        |> htmlsToRow
