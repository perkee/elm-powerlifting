module Data.ColumnToggles exposing
    ( Config(..)
    , State
    , columns
    , config
    , init
    , subscriptions
    , title
    , toggleColumn
    , updateAccordion
    )

import Bootstrap.Accordion as Accordion
import Column exposing (Column, allColumns)
import Library exposing (filterListByList)
import Platform.Sub


type alias State =
    { accordionState : Accordion.State
    , columns : List Column
    }


type Config msg
    = Config (ConfigRec msg)


type alias ConfigRec msg =
    { id : String
    , title : String
    , toMsg : State -> msg
    }


config : (State -> msg) -> String -> Config msg
config toMsg id =
    -- since ID is the only required field no need for a record type
    Config
        { toMsg = toMsg
        , id = id
        , title = ""
        }


title : String -> Config msg -> Config msg
title title_ (Config configRec) =
    Config { configRec | title = title_ }


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
