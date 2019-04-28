module Main exposing (main)

-- (Html, button, div, text, input, option, select)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, targetValue)
import Json.Decode as Json


main =
    Browser.sandbox { init = init, update = update, view = view }


coefficients : Sex -> List Float
coefficients sex =
    case sex of
        Male ->
            [ -216.0475144
            , 16.2606339
            , -0.002388645
            , -0.00113732
            , 7.01863e-6
            , -1.291e-8
            ]

        Female ->
            [ 594.31747775582
            , -27.23842536447
            , 0.82112226871
            , -0.00930733913
            , 4.731582e-5
            , -9.054e-8
            ]


mult : Float -> Int -> Float -> Float
mult bw i c =
    c * bw ^ toFloat i


type ModelInKilos
    = Complete
        { bodyMass : Float
        , liftedMass : Float
        , sex : Sex
        }
    | Incomplete


massToKilos : MassUnit -> Float -> Float
massToKilos u m =
    case u of
        KG ->
            m

        LBM ->
            m / 2.204623


modelToKilos : Model -> ModelInKilos
modelToKilos m =
    case m.bodyMass.value of
        Just bodyMass ->
            case m.liftedMass.value of
                Just liftedMass ->
                    Complete
                        { bodyMass = massToKilos m.bodyUnit bodyMass
                        , liftedMass = massToKilos m.liftedUnit liftedMass
                        , sex = m.sex
                        }

                Nothing ->
                    Incomplete

        Nothing ->
            Incomplete


wilks : Model -> String
wilks model =
    case modelToKilos model of
        Complete m ->
            m.sex
                |> coefficients
                |> List.indexedMap (mult m.bodyMass)
                |> List.foldl (+) 0
                |> (\denom ->
                        m.liftedMass
                            * 500
                            / denom
                            |> String.fromFloat
                   )

        Incomplete ->
            "Incomplete info to render a wilks score"


deeplyNestedIrritatingWilks : Model -> String
deeplyNestedIrritatingWilks m =
    case m.bodyMass.value of
        Just bodyMass ->
            case m.liftedMass.value of
                Just lifted ->
                    m.sex
                        |> coefficients
                        |> List.indexedMap (mult bodyMass)
                        |> List.foldl (+) 0
                        |> (\denom ->
                                lifted
                                    * 500
                                    / denom
                                    |> String.fromFloat
                           )

                Nothing ->
                    "no lifted!"

        Nothing ->
            "No Body mass"


type Kilos
    = Kilos Float


type MassUnit
    = KG
    | LBM


type Sex
    = Male
    | Female


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


unitDecoder : Json.Decoder MassUnit
unitDecoder =
    -- so we try to decode the string and put the result into stringToUnitDecoder
    targetValue
        |> Json.andThen stringToUnitDecoder


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


sexDecoder : Json.Decoder Sex
sexDecoder =
    -- so we try to decode the string and put the result into stringToSexDecoder
    targetValue
        |> Json.andThen stringToSexDecoder


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
        [ on "change" <| Json.map m unitDecoder
        , value <| unitToValue u
        ]
        [ unitOption KG
        , unitOption LBM
        ]


view : Model -> Html Msg
view model =
    div []
        [ label [ for "sexInput" ] [ text "A " ]
        , select
            [ on "change" (Json.map SetSex sexDecoder)
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
        , div [] [ wilks model |> text ]
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
