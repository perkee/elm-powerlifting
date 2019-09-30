module View.Cards exposing (view)

import Data.Cards as Cards
import Data.ColumnToggles as ColumnToggles
import Html exposing (Html)
import SavedFeat exposing (SavedFeat)
import View.FeatCards as FeatCards
import View.ScoreCards as ScoreCards


view :
    List SavedFeat
    -> ColumnToggles.State
    -> Cards.State
    -> FeatCards.CardMsgs msg
    -> List (Html msg)
view savedFeats tableState cardsState cardMsgs =
    if List.isEmpty savedFeats then
        []

    else
        ScoreCards.view
            savedFeats
            (ScoreCards.State cardsState.scoreMassUnit tableState)
            cardsState
            cardMsgs.cardsChanged
            cardMsgs.noteChanged
            ++ FeatCards.view savedFeats
                tableState
                cardsState
                cardMsgs
