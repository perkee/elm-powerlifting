module Dropdowns exposing (Option, typedSelect)

import Bootstrap.Form.Select as Select
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


optionsToMessenger : List (Option t) -> String -> Maybe t
optionsToMessenger options val =
    options
        |> List.map (\option -> ( option.valAttr, option.value ))
        |> Dict.fromList
        |> Dict.get val


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


typedSelect : List (Select.Option msg) -> List (Option t) -> t -> (Maybe t -> msg) -> H.Html msg
typedSelect attrs options current jsonMapper =
    Select.select
        (Select.onChange (optionsToMessenger options >> jsonMapper)
            :: attrs
        )
        (List.map
            (opt current)
            options
        )


opt : t -> Option t -> Select.Item msg
opt current option =
    Select.item
        [ option.valAttr |> value
        , current == option.value |> selected
        ]
        [ option.label |> H.text ]
