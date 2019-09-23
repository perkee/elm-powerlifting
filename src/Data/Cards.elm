module Data.Cards exposing (State, init)

import Feat
import Library
import SortColumn exposing (SortColumn(..))


type alias State =
    { sortColumn : SortColumn.SortColumn
    , sortOrder : Library.SortOrder
    , scoreMassUnit : Feat.MassUnit
    }


init : State
init =
    State SortColumn.Index
        Library.Ascending
        Feat.KG
