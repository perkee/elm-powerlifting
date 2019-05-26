module View.UnitDropdown exposing (view)

import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Form.InputGroup as InputGroup
import Data.UnitDropdown exposing (State)
import Feat exposing (MassUnit(..))
import Html exposing (text)
import Html.Events exposing (onClick)


view : State -> (State -> msg) -> InputGroup.Addon msg
view state updateMsg =
    InputGroup.dropdown
        state.dropdownState
        { options = []
        , toggleMsg = \ddState -> { state | dropdownState = ddState } |> updateMsg
        , toggleButton =
            (case state.massUnit of
                KG ->
                    "Kilos"

                LBM ->
                    "Pounds"
            )
                |> text
                |> List.singleton
                |> Dropdown.toggle [ Button.primary, Button.small ]
        , items =
            [ Dropdown.buttonItem [ onClick <| ({ state | massUnit = KG } |> updateMsg) ] [ text "Kilos" ]
            , Dropdown.buttonItem [ onClick <| ({ state | massUnit = LBM } |> updateMsg) ] [ text "Pounds" ]
            ]
        }
