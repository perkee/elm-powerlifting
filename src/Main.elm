module Main exposing (main)

-- (Html, button, div, text, input, option, select)

import Browser
import Dropdowns exposing (Option, typedSelect)
import Feat exposing (Feat, Gender(..), Lift(..), MassUnit(..), massToKilos)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, targetValue)
import Json.Decode as Json
import Scores exposing (scores, scoresToPara, scoresToTable, scoresToText)


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
    , feats : List Feat
    }


init : Model
init =
    { liftedMass = initFloatField
    , liftedUnit = LBM
    , bodyMass = initFloatField
    , bodyUnit = LBM
    , gender = Male
    , lift = Total
    , feats = []
    }


modelToFeat : Model -> Maybe Feat
modelToFeat m =
    case ( m.bodyMass.value, m.liftedMass.value ) of
        ( Just bodyMass, Just liftedMass ) ->
            Just
                { bodyKilos = massToKilos m.bodyUnit bodyMass
                , liftedKilos = massToKilos m.liftedUnit liftedMass
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
    | SaveFeat


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

        SaveFeat ->
            case modelToFeat model of
                Just feat ->
                    { model | feats = feat :: model.feats }

                Nothing ->
                    model


modelToScoresDom : Model -> Html msg
modelToScoresDom m =
    div []
        [ m |> modelToFeat |> scoresToTable
        , m |> modelToFeat |> scoresToPara
        , text "here comes the list"
        , m.feats
            |> List.map
                (Just
                    >> scoresToText
                    >> List.singleton
                    >> li []
                )
            |> ol []
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
            [ onClick SaveFeat
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
