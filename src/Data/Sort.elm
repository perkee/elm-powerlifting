module Data.Sort exposing (State, init, kindaFlip, setMaybeColumn, toggleOrder)

import Library
import SortColumn


type alias State =
    { sortColumn : SortColumn.SortColumn
    , sortOrder : Library.SortOrder
    }


init : State
init =
    { sortColumn = SortColumn.Index
    , sortOrder = Library.Ascending
    }


kindaFlip : State -> SortColumn.SortColumn -> State
kindaFlip s column =
    let
        state =
            { s | sortColumn = column }
    in
    if state.sortColumn /= column then
        { state | sortOrder = Library.Descending }

    else
        case state.sortOrder of
            Library.Ascending ->
                { state | sortOrder = Library.Descending }

            Library.Descending ->
                { state | sortOrder = Library.Ascending }


setMaybeColumn : State -> Maybe SortColumn.SortColumn -> State
setMaybeColumn state maybeCol =
    case maybeCol of
        Just col ->
            { state | sortColumn = col }

        Nothing ->
            state


toggleOrder : State -> State
toggleOrder state =
    { state
        | sortOrder =
            case state.sortOrder of
                Library.Ascending ->
                    Library.Descending

                Library.Descending ->
                    Library.Ascending
    }
