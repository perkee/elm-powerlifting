module Dropdowns exposing (sexSelect, unitSelect)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, targetValue)
import Json.Decode as Json
import ModelInKilos exposing (Lift(..), MassUnit(..), ModelInKilos, Sex(..), massToKilos)


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
            "kilos"

        LBM ->
            "pounds"


stringToSexDecoder : String -> Json.Decoder Sex
stringToSexDecoder s =
    case s of
        "M" ->
            Json.succeed Male

        "F" ->
            Json.succeed Female

        x ->
            Json.fail <| "unknown sex" ++ x


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
            "man"

        Female ->
            "woman"


opt : (a -> String) -> (a -> String) -> a -> a -> Html msg
opt valToValue valToLabel current val =
    option
        [ val |> valToValue |> value
        , current == val |> selected
        ]
        [ val |> valToLabel |> text ]


unitOption : MassUnit -> MassUnit -> Html msg
unitOption =
    opt unitToValue unitToLabel


sexOption : Sex -> Sex -> Html msg
sexOption =
    opt sexToValue sexToLabel


onChange : (String -> Json.Decoder selectableType) -> (selectableType -> msg) -> Html.Attribute msg
onChange stringToDecoder mapper =
    on "change" <| Json.map mapper <| decoder stringToDecoder


unitSelect : MassUnit -> (MassUnit -> msg) -> Html msg
unitSelect unit m =
    select
        [ --on "change" <| Json.map m <| decoder stringToUnitDecoder
          onChange stringToUnitDecoder m
        ]
        [ unitOption unit KG
        , unitOption unit LBM
        ]


sexSelect : Sex -> (Sex -> msg) -> Html msg
sexSelect sex m =
    select
        [ --on "change" <| Json.map m <| decoder stringToSexDecoder
          onChange stringToSexDecoder m
        , id "sexInput"
        ]
        [ sexOption sex Male
        , sexOption sex Female
        ]
