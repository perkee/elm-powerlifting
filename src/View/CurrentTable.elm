module View.CurrentTable exposing (view)

import Column
    exposing
        ( Column
        , columnToToggleLabel
        )
import Feat exposing (Feat)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HSA
import Renderer exposing (rowsToHeadedTable)
import SavedFeat exposing (SavedFeat)
import Scores
    exposing
        ( featToRecord
        , maxRecord
        )


view : List SavedFeat -> List Column -> Feat -> H.Html msg
view savedFeats cols feat =
    List.map (featToColumnToRow savedFeats feat) cols
        |> rowsToHeadedTable
            [ ( "Label", H.text "" )
            , ( "Value", H.text "" )
            ]


featToColumnToRow :
    List SavedFeat
    -> Feat
    -> Column
    -> ( String, Html msg )
featToColumnToRow savedFeats feat column =
    ( columnToToggleLabel column
    , H.tr
        []
        [ column
            |> columnToToggleLabel
            |> H.text
            |> List.singleton
            |> H.td [ "body-cell--label" |> HSA.class ]
        , columnToFeatToText savedFeats column feat
            |> List.singleton
            |> H.td [ "body-cell--value" |> HSA.class ]
        ]
    )


columnToFeatToText : List SavedFeat -> Column -> Feat -> Html msg
columnToFeatToText savedFeats col =
    featToRecord >> columnToRecordToText savedFeats col


columnToRecordToText :
    List SavedFeat
    -> Column
    -> Scores.Record
    -> Html msg
columnToRecordToText savedFeats col =
    (case SavedFeat.maxRecord savedFeats of
        Nothing ->
            Column.columnToRecordToText col

        Just mr ->
            Column.columnToRecordToTextWithMaxes mr col
    )
        >> H.fromUnstyled
