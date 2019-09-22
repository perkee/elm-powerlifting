module View.Cards exposing (view)

import Data.ColumnToggles as ColumnToggles
import Html exposing (Html)
import SavedFeat exposing (SavedFeat)
import View.FeatCards as FeatCards
import View.ScoreCards as ScoreCards


view : List SavedFeat -> ColumnToggles.State -> FeatCards.CardSorting msg -> List (Html msg)
view savedFeats tableState cardSorting =
    if List.isEmpty savedFeats then
        []

    else
        -- List SavedFeat -> State -> msg -> NoteChangedMsg msg -> List (Html msg)
        ScoreCards.view
            savedFeats
            (ScoreCards.State cardSorting.scoreMassUnit tableState)
            cardSorting.massUnitMsg
            cardSorting.noteChanged
            ++ FeatCards.view savedFeats
                tableState
                cardSorting
