module Main exposing (main)

-- (Html, button, div, text, input, option, select)

import Browser
import Dropdowns exposing (Option, typedSelect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, targetValue)
import Json.Decode as Json
import ModelInKilos exposing (Lift(..), MassUnit(..), ModelInKilos, Gender(..), massToKilos)
import Scores exposing (scores, scoresToPara, scoresToTable)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias FloatField =
    { value : Maybe Float
    , input : String
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
    }


init : Model
init =
    Model initFloatField LBM initFloatField LBM Female Total


modelToKilos : Model -> ModelInKilos
modelToKilos m =
    case ( m.bodyMass.value, m.liftedMass.value ) of
        ( Just bodyMass, Just liftedMass ) ->
            Just
                { bodyMass = massToKilos m.bodyUnit bodyMass
                , liftedMass = massToKilos m.liftedUnit liftedMass
                , gender = m.gender
                , lift = m.lift
                }

        ( _, _ ) ->
            Nothing


type Msg
    = SetLiftedMass String
    | SetLiftedUnit MassUnit
    | SetBodyMass String
    | SetBodyUnit MassUnit
    | SetGender Gender
    | SetLift Lift


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


modelToScoresDom : Model -> Html msg
modelToScoresDom m =
    div []
        [ m |> modelToKilos |> scoresToTable
        , m |> modelToKilos |> scoresToPara
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
