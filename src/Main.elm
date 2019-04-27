module Main exposing (FloatField, MassUnit(..), Model, Msg(..), Sex(..), abcdef, ffParse, ffValue, init, main, mult, opt, sexDecoder, sexOption, sexToLabel, sexToValue, stringToSexDecoder, stringToUnitDecoder, unitDecoder, unitOption, unitSelect, unitToLabel, unitToValue, update, view, viewInput, viewInputFloat, wilks)

-- (Html, button, div, text, input, option, select)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, targetValue)
import Json.Decode as Json


main =
    Browser.sandbox { init = init, update = update, view = view }


abcdef : Sex -> List Float
abcdef sex =
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


wilks : Sex -> Maybe Float -> Maybe Float -> String
wilks sex mbw mw =
    sex
        |> abcdef
        |> List.indexedMap (mult (Maybe.withDefault 0 mbw))
        |> List.foldl (+) 0
        |> (\denom ->
                Maybe.withDefault 0 mw
                    * 500
                    / denom
                    |> String.fromFloat
           )


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


type alias Model =
    { mass : FloatField
    , massUnit : MassUnit
    , bodyMass : FloatField
    , bodyMassUnit : MassUnit
    , sex : Sex
    }


init : Model
init =
    Model (FloatField (Just 0.0) "0") LBM (FloatField (Just 0.0) "0") LBM Male


type Msg
    = SetMass String
    | SetBodyMass String
    | SetMassUnit MassUnit
    | SetBodyMassUnit MassUnit
    | SetSex Sex


ffValue : FloatField -> Float
ffValue ff =
    ff.value |> Maybe.withDefault 0


ffParse : FloatField -> String -> FloatField
ffParse ff str =
    case String.toFloat str of
        Nothing ->
            FloatField Nothing str

        Just new ->
            FloatField (Just new) str


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetMass s ->
            { model | mass = ffParse model.mass s }

        SetBodyMass s ->
            { model | bodyMass = ffParse model.bodyMass s }

        SetMassUnit u ->
            { model | massUnit = u }

        SetBodyMassUnit u ->
            { model | bodyMassUnit = u }

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


unitSelect : (MassUnit -> Msg) -> Html Msg
unitSelect m =
    select
        [ on "change" (Json.map m unitDecoder) ]
        [ unitOption KG
        , unitOption LBM
        ]


view : Model -> Html Msg
view model =
    div []
        [ select
            [ on "change" (Json.map SetSex sexDecoder)
            ]
            [ sexOption Male
            , sexOption Female
            ]
        , viewInput "number" "0" model.mass.input SetMass
        , unitSelect SetMassUnit
        , viewInput "number" "0" model.bodyMass.input SetBodyMass
        , unitSelect SetBodyMassUnit
        , span [] [ wilks model.sex model.mass.value model.bodyMass.value |> text ]
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewInputFloat : String -> Float -> (Float -> msg) -> Html msg
viewInputFloat p v toMsg =
    viewInput "number" p (String.fromFloat v) (String.toFloat >> Maybe.withDefault 0.0 >> toMsg)
