module View.Cards exposing (view)

import Css
import Data.Cards as Cards exposing (Display(..), State, toggleDisplay)
import Data.ColumnToggles as ColumnToggles
import Html as H exposing (Html)
import Html.Styled as HS
import Html.Styled.Attributes as HSA
import Html.Styled.Events as HE
import SavedFeat exposing (IndexedSavedFeat)
import View.FeatCards as FeatCards
import View.ScoreCards as ScoreCards


view :
    List IndexedSavedFeat
    -> ColumnToggles.State
    -> State
    -> FeatCards.CardMsgs msg
    -> List (H.Html msg)
view savedFeats tableState cardsState cardMsgs =
    case savedFeats of
        [] ->
            []

        _ ->
            viewHeadline cardsState cardMsgs
                :: viewCards savedFeats tableState cardsState cardMsgs


viewHeadline :
    State
    -> FeatCards.CardMsgs msg
    -> Html msg
viewHeadline cardsState cardMsgs =
    HS.h3 [ HSA.class "d-md-none" ]
        [ HS.text "Grouped by "
        , HS.button
            [ HSA.class "btn btn-outline-secondary btn-sm"
            , HE.onClick <| cardMsgs.cardsChanged <| toggleDisplay cardsState
            , HSA.css
                [ Css.fontSize <| Css.rem 1.75
                ]
            ]
            [ HS.text <|
                case cardsState.display of
                    ByFeat ->
                        "Instance"

                    ByScore ->
                        "Score"
            ]
        ]
        |> HS.toUnstyled


viewCards :
    List IndexedSavedFeat
    -> ColumnToggles.State
    -> State
    -> FeatCards.CardMsgs msg
    -> List (Html msg)
viewCards savedFeats tableState cardsState cardMsgs =
    case cardsState.display of
        Cards.ByFeat ->
            FeatCards.view savedFeats
                tableState
                cardsState
                cardMsgs

        Cards.ByScore ->
            ScoreCards.view
                savedFeats
                (ScoreCards.State cardsState.scoreMassUnit tableState)
                cardsState
                cardMsgs
                |> List.singleton
