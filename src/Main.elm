module Main exposing (main)

-- (Html, button, div, text, input, option, select)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, targetValue)
import Json.Decode as Json
import ModelInKilos exposing (MassUnit(..), ModelInKilos, Sex(..), massToKilos)
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
    Model initFloatField LBM initFloatField LBM Male


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


decoder : (String -> Json.Decoder selectableType) -> Json.Decoder selectableType
decoder stringDecoder =
    targetValue
        |> Json.andThen stringDecoder


stringToUnitDecoder : String -> Json.Decoder MassUnit
stringToUnitDecoder s =
    case s of
        "KG" ->
            Json.succeed KG

        "LBM" ->
            Json.succeed LBM

        x ->
            Json.fail ("unknown unit" ++ x)


unitToValue : MassUnit -> String
unitToValue mu =
    case mu of
        KG ->
            "KG"

        LBM ->
            "LBM"


unitToLabel : MassUnit -> String
unitToLabel mu =
    case mu of
        KG ->
            "Kilos"

        LBM ->
            "Pounds"


stringToSexDecoder : String -> Json.Decoder Sex
stringToSexDecoder s =
    case s of
        "M" ->
            Json.succeed Male

        "F" ->
            Json.succeed Female

        x ->
            Json.fail ("unknown sex" ++ x)


sexToValue : Sex -> String
sexToValue s =
    case s of
        Male ->
            "M"

        Female ->
            "F"


sexToLabel : Sex -> String
sexToLabel s =
    case s of
        Male ->
            "Man"

        Female ->
            "Woman"


opt : (a -> String) -> (a -> String) -> a -> Html msg
opt valToValue valToLabel val =
    option [ val |> valToValue |> value ] [ val |> valToLabel |> text ]


unitOption : MassUnit -> Html msg
unitOption =
    opt unitToValue unitToLabel


sexOption : Sex -> Html msg
sexOption =
    opt sexToValue sexToLabel


unitSelect : MassUnit -> (MassUnit -> Msg) -> Html Msg
unitSelect u m =
    select
        [ on "change" <| Json.map m <| decoder stringToUnitDecoder
        , value <| unitToValue u
        ]
        [ unitOption KG
        , unitOption LBM
        ]


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
        , select
            [ on "change" <| Json.map SetSex <| decoder stringToSexDecoder
            , id "sexInput"
            ]
            [ sexOption Male
            , sexOption Female
            ]
        , label [ for "liftedInput" ] [ text " lifted " ]
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
        , value v
        , onInput toMsg
        ]
        []
