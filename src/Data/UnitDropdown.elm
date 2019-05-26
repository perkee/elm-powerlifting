module Data.UnitDropdown exposing (State, init, toKilos, toPounds, toSubs)

import Bootstrap.Dropdown as Dropdown
import Feat exposing (MassUnit(..), massToKilos, massToPounds)
import Platform.Sub


type alias State =
    { dropdownState : Dropdown.State
    , massUnit : MassUnit
    }


toKilos : State -> Float -> Float
toKilos state =
    massToKilos state.massUnit


toPounds : State -> Float -> Float
toPounds state =
    massToPounds state.massUnit


init : State
init =
    { dropdownState = Dropdown.initialState
    , massUnit = KG
    }


toSubs : State -> (State -> msg) -> Platform.Sub.Sub msg
toSubs state msg =
    Dropdown.subscriptions state.dropdownState (\x -> msg { state | dropdownState = x })
