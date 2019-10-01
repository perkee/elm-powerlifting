module Data.Cards exposing (Display(..), State, init, setSort, toggleDisplay, toggleMassUnit)

import Data.Sort as Sort
import Feat


type Display
    = ByFeat
    | ByScore


type alias State =
    { sort : Sort.State
    , scoreMassUnit : Feat.MassUnit
    , display : Display
    }


init : Sort.State -> State
init sort =
    State sort Feat.KG ByFeat


setSort : State -> Sort.State -> State
setSort state sort =
    { state | sort = sort }


toggleMassUnit : State -> State
toggleMassUnit state =
    { state
        | scoreMassUnit = Feat.toggleMassUnit state.scoreMassUnit
    }


toggleDisplay : State -> State
toggleDisplay state =
    { state
        | display =
            case state.display of
                ByFeat ->
                    ByScore

                ByScore ->
                    ByFeat
    }
