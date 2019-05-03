module Dropdowns exposing (Option, typedSelect)

import Dict
import Html as H
import Html.Attributes exposing (..)
import Html.Events exposing (on, onInput, targetValue)
import Json.Decode as Json


type alias Option t =
    { value : t
    , label : String
    , valAttr : String
    }


optionsToDecoder : List (Option t) -> String -> Json.Decoder t
optionsToDecoder options val =
    case
        options
            |> List.map (\option -> ( option.valAttr, option.value ))
            |> Dict.fromList
            |> Dict.get val
    of
        Just v ->
            Json.succeed v

        Nothing ->
            Json.fail ("unknown option: " ++ val)


findVal : t -> Option t -> String -> String
findVal current option default =
    if option.value == current then
        option.label

    else
        default


optionsToLabeler : List (Option t) -> t -> String
optionsToLabeler options val =
    List.foldl
        (findVal val)
        "unknown option"
        options


typedSelect : List (Option t) -> t -> (t -> msg) -> H.Html msg
typedSelect options current jsonMapper =
    H.select
        [ targetValue
            |> Json.andThen (optionsToDecoder options)
            |> Json.map jsonMapper
            |> on "change"
        ]
        (List.map
            (opt current)
            options
        )


opt : t -> Option t -> H.Html msg
opt current option =
    H.option
        [ option.valAttr |> value
        , current == option.value |> selected
        ]
        [ option.label |> H.text ]
