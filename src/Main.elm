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
import Library exposing (thrush, updateArrayAt)
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
        , m.feats |> savedFeatsToTable
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


savedFeatsToTable : Array SavedFeat -> Html Msg
savedFeatsToTable =
    Array.indexedMap savedFeatToRow
        >> Array.toList
        >> rowsToHeadedTable [ "Index", "Note", "Lift (kg)", "BW (kg)", "Lift (lb)", "BW (lb)", "Wilks", "Scaled Allometric", "Allometric", "IPF", "McCulloch" ]


savedFeatToRow : Int -> SavedFeat -> Html Msg
savedFeatToRow index savedFeat =
    [ [ .index >> String.fromInt >> text
      , .note >> (\v -> input [ type_ "text", placeholder "Note", value v, onInput (SetNote index) ] [])
      ]
        |> List.map (thrush savedFeat)
    , savedFeat.feat |> featToRecord |> recordToCells
    ]
        |> List.concat
        |> htmlsToRow
