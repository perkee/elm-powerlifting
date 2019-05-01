module Main exposing (main)

-- (Html, button, div, text, input, option, select)

import Browser
import Dropdowns exposing (sexSelect, unitSelect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, targetValue)
import Json.Decode as Json
import ModelInKilos exposing (Lift(..), MassUnit(..), ModelInKilos, Sex(..), massToKilos)
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
    , sex : Sex
    }


init : Model
init =
    Model initFloatField LBM initFloatField LBM Female


modelToKilos : Model -> ModelInKilos
modelToKilos m =
    case ( m.bodyMass.value, m.liftedMass.value ) of
        ( Just bodyMass, Just liftedMass ) ->
            Just
                { bodyMass = massToKilos m.bodyUnit bodyMass
                , liftedMass = massToKilos m.liftedUnit liftedMass
                , sex = m.sex
                }

        ( _, _ ) ->
            Nothing


type Msg
    = SetLiftedMass String
    | SetLiftedUnit MassUnit
    | SetBodyMass String
    | SetBodyUnit MassUnit
    | SetSex Sex


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

        SetSex s ->
            { model | sex = s }


modelToScoresDom : Model -> Html msg
modelToScoresDom m =
    let
        table =
            m |> modelToKilos |> scoresToTable

        para =
            m |> modelToKilos |> scoresToPara
    in
    div []
        [ table
        , para
        ]


view : Model -> Html Msg
view model =
    div []
        [ label [ for "sexInput" ] [ text "A " ]
        , sexSelect model.sex SetSex
        , label [ for "liftedInput" ] [ text " totaled " ]
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
