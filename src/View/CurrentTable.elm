module View.CurrentTable exposing (view)

import Column
    exposing
        ( Column
        , columnToRecordToText
        , columnToRecordToTextWithMaxes
        , columnToToggleLabel
        )
import Feat exposing (Feat)
import Html as H exposing (Html)
import Html.Styled
import Html.Styled.Attributes as HSA
import Library exposing (SortOrder(..), thrush)
import Renderer exposing (rowsToHeadedTable)
import SavedFeat exposing (SavedFeat)
import Scores
    exposing
        ( featToRecord
        , maxRecord
        )


view : List SavedFeat -> List Column -> Feat -> Html msg
view savedFeats cols =
    let
        recordsToText =
            cols
                |> List.map
                    (case SavedFeat.maxRecord savedFeats of
                        Nothing ->
                            \c r -> columnToRecordToText c r |> Html.Styled.fromUnstyled

                        Just mr ->
                            \c r -> columnToRecordToTextWithMaxes mr c r |> Html.Styled.fromUnstyled
                    )
    in
    featToRecord
        >> thrush
        >> List.map
        >> thrush recordsToText
        >> List.map2
            (\label value ->
                ( label |> columnToToggleLabel
                , Html.Styled.tr
                    []
                    [ label |> columnToToggleLabel |> Html.Styled.text |> List.singleton |> Html.Styled.td [ "body-cell--label" |> HSA.class ]
                    , value |> List.singleton |> Html.Styled.td [ "body-cell--value" |> HSA.class ]
                    ]
                )
            )
            cols
        >> List.map (Tuple.mapSecond Html.Styled.toUnstyled)
        >> rowsToHeadedTable [ ( "Label", H.text "" ), ( "Value", H.text "" ) ]
