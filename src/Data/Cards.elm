module Data.Cards exposing (State, init, setSort, toggleMassUnit)

import Data.Sort as Sort
import Feat


type alias State =
    { sort : Sort.State
    , scoreMassUnit : Feat.MassUnit
    }


init : Sort.State -> State
init sort =
    State sort Feat.KG


setSort : State -> Sort.State -> State
setSort state sort =
    { state | sort = sort }


toggleMassUnit : State -> State
toggleMassUnit state =
    { state
        | scoreMassUnit = Feat.toggleMassUnit state.scoreMassUnit
    }
