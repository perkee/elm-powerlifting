module Data.ColumnToggles exposing (State, columns, init, subscriptions, toggleColumn, updateAccordion)

import Bootstrap.Accordion as Accordion
import Column exposing (Column, allColumns)
import Library exposing (filterListByList)
import Platform.Sub


type alias State =
    { accordionState : Accordion.State
    , columns : List Column
    }


init : List Column -> State
init cols =
    { accordionState = Accordion.initialState
    , columns = cols
    }


updateAccordion : State -> Accordion.State -> State
updateAccordion state accordionState =
    { state | accordionState = accordionState }


toggleColumn : State -> Column -> Bool -> State
toggleColumn state col checked =
    { state
        | columns =
            if checked then
                col :: state.columns

            else
                List.filter ((/=) col) state.columns
    }


columns : State -> List Column
columns state =
    filterListByList state.columns allColumns


subscriptions : State -> (State -> msg) -> Platform.Sub.Sub msg
subscriptions state msg =
    Sub.batch
        [ Accordion.subscriptions state.accordionState
            ((\x -> { state | accordionState = x })
                >> msg
            )
        ]
