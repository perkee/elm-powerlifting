module Main exposing (main)

-- (Html, button, div, text, input, option, select)

import Array exposing (Array)
import Browser
import Dropdowns exposing (Option, typedSelect)
import Feat exposing (Feat, Gender(..), Lift(..), MassUnit(..), massToKilos, massToPounds)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, targetValue)
import Json.Decode as Json
import Json.Encode as JE
import Renderer exposing (rowsToHeadedTable, textual)
import Scores exposing (Record, featToRecord, recordToPara, recordToString, recordToTable, recordToText)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias FloatField =
    { value : Maybe Float
    , input : String
    }


type alias SavedRecord =
    { record : Record
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
    , records : Array SavedRecord
    }


init : Model
init =
    { liftedMass = initFloatField
    , liftedUnit = LBM
    , bodyMass = initFloatField
    , bodyUnit = LBM
    , gender = Male
    , lift = Total
    , records = Array.empty
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
                }

        ( _, _ ) ->
            Nothing


modelToRecord : Model -> Maybe Record
modelToRecord =
    modelToFeat >> featToRecord


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
    | SaveRecord
    | SetRecordNote Int String


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


updateArrayAt : Int -> (a -> a) -> Array a -> Array a
updateArrayAt index fn array =
    case Array.get index array of
        Just value ->
            Array.set index (fn value) array

        Nothing ->
            array


setNoteOnSavedRecord : String -> SavedRecord -> SavedRecord
setNoteOnSavedRecord note record =
    { record | note = note }


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

        SaveRecord ->
            case model |> modelToFeat |> featToRecord of
                Just record ->
                    { model | records = Array.push (SavedRecord record (Array.length model.records) "") model.records }

                Nothing ->
                    model

        SetRecordNote index note ->
            { model | records = updateArrayAt index (setNoteOnSavedRecord note) model.records }


modelToScoresDom : Model -> Html Msg
modelToScoresDom m =
    div []
        [ m |> modelToRecord |> recordToTable
        , m |> modelToRecord |> recordToPara
        , m.records
            |> Array.map
                (.record
                    >> recordToString
                    >> text
                    >> List.singleton
                    >> li []
                )
            >> Array.toList
            |> ol []
        , m.records |> savedRecordsToTable
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
            ]
            model.gender
            SetGender
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
        , button
            [ onClick SaveRecord
            , model |> modelToFeat |> canMakeFeat |> not |> disabled
            ]
            [ text "save" ]
        , modelToScoresDom model
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewFloatInput : String -> String -> (String -> msg) -> Html msg
viewFloatInput id v toMsg =
    input
        [ Html.Attributes.id id
        , type_ "number"
        , placeholder "0"
        , onInput toMsg
        ]
        []


htmlsToRow : List (Html msg) -> Html msg
htmlsToRow =
    List.map (List.singleton >> td [])
        >> tr []


thrush : a -> (a -> b) -> b
thrush input fn =
    fn input


savedRecordsToTable : Array SavedRecord -> Html Msg
savedRecordsToTable =
    Array.indexedMap savedRecordToRow
        >> Array.toList
        >> rowsToHeadedTable [ "Index", "String", "Note" ]


savedRecordToRow : Int -> SavedRecord -> Html Msg
savedRecordToRow index savedRecord =
    [ .index >> String.fromInt >> text
    , .note >> (\v -> input [ type_ "text", placeholder "Note", value v, onInput (SetRecordNote index) ] [])
    , .record >> recordToString >> text
    ]
        |> List.map (thrush savedRecord)
        |> htmlsToRow
