module View.Cards exposing (view)

import Css
import Data.Cards as Cards exposing (Display(..), State, toggleDisplay)
import Data.ColumnToggles as ColumnToggles
import Html as H exposing (Html)
import Html.Styled as HS
import Html.Styled.Attributes as HSA
import Html.Styled.Events as HE
import SavedFeat exposing (SavedFeat)
import View.FeatCards as FeatCards
import View.ScoreCards as ScoreCards


view :
    List SavedFeat
    -> ColumnToggles.State
    -> State
    -> FeatCards.CardMsgs msg
    -> List (H.Html msg)
view savedFeats tableState cardsState cardMsgs =
    ([ HS.h3 [ HSA.class "d-md-none" ]
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
     ]
        |> List.map HS.toUnstyled
    )
        ++ viewCards savedFeats tableState cardsState cardMsgs


viewCards :
    List SavedFeat
    -> ColumnToggles.State
    -> State
    -> FeatCards.CardMsgs msg
    -> List (Html msg)
viewCards savedFeats tableState cardsState cardMsgs =
    case ( savedFeats, cardsState.display ) of
        ( [], _ ) ->
            []

        ( sf, Cards.ByFeat ) ->
            FeatCards.view sf
                tableState
                cardsState
                cardMsgs

        ( sf, Cards.ByScore ) ->
            ScoreCards.view
                sf
                (ScoreCards.State cardsState.scoreMassUnit tableState)
                cardsState
                cardMsgs
                |> List.singleton
