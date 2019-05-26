module Dropdowns exposing (Option, typedSelect)

import Bootstrap.Form.Select as Select
import Dict
import Html as H
import Html.Attributes as HA


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
        [ option.valAttr |> HA.value
        , current == option.value |> HA.selected
        ]
        [ option.label |> H.text ]
