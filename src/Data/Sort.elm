module Data.Sort exposing (Status, init, kindaFlip, setMaybeColumn, toggleOrder)

import Library
import SortColumn


type alias Status =
    { sortColumn : SortColumn.SortColumn
    , sortOrder : Library.SortOrder
    }


init : Status
init =
    { sortColumn = SortColumn.Index
    , sortOrder = Library.Ascending
    }


kindaFlip : Status -> SortColumn.SortColumn -> Status
kindaFlip s column =
    let
        status =
            { s | sortColumn = column }
    in
    if status.sortColumn /= column then
        { status | sortOrder = Library.Descending }

    else
        case status.sortOrder of
            Library.Ascending ->
                { status | sortOrder = Library.Descending }

            Library.Descending ->
                { status | sortOrder = Library.Ascending }


setMaybeColumn : Status -> Maybe SortColumn.SortColumn -> Status
setMaybeColumn status maybeCol =
    case maybeCol of
        Just col ->
            { status | sortColumn = col }

        Nothing ->
            status


toggleOrder : Status -> Status
toggleOrder status =
    { status
        | sortOrder =
            case status.sortOrder of
                Library.Ascending ->
                    Library.Descending

                Library.Descending ->
                    Library.Ascending
    }
