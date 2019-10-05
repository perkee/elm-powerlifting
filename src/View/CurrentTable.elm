module View.CurrentTable exposing (view)

import Column
    exposing
        ( Column
        , columnToToggleLabel
        )
import Feat exposing (Feat)
import Html as H exposing (Html)
import Html.Styled
import Html.Styled.Attributes as HSA
import Renderer exposing (rowsToHeadedTable)
import SavedFeat exposing (SavedFeat)
import Scores
    exposing
        ( featToRecord
        , maxRecord
        )


view : List SavedFeat -> List Column -> Feat -> Html msg
view savedFeats cols feat =
    List.map (featToColumnToRow savedFeats feat) cols
        |> rowsToHeadedTable [ ( "Label", H.text "" ), ( "Value", H.text "" ) ]


featToColumnToRow : List SavedFeat -> Feat -> Column -> ( String, Html msg )
featToColumnToRow savedFeats feat column =
    ( columnToToggleLabel column
    , Html.Styled.tr
        []
        [ column
            |> columnToToggleLabel
            |> Html.Styled.text
            |> List.singleton
            |> Html.Styled.td [ "body-cell--label" |> HSA.class ]
        , columnToFeatToText savedFeats column feat
            |> List.singleton
            |> Html.Styled.td [ "body-cell--value" |> HSA.class ]
        ]
        |> Html.Styled.toUnstyled
    )


columnToFeatToText : List SavedFeat -> Column -> Feat -> Html.Styled.Html msg
columnToFeatToText savedFeats col =
    featToRecord >> columnToRecordToText savedFeats col


columnToRecordToText : List SavedFeat -> Column -> Scores.Record -> Html.Styled.Html msg
columnToRecordToText savedFeats col =
    (case SavedFeat.maxRecord savedFeats of
        Nothing ->
            Column.columnToRecordToText col

        Just mr ->
            Column.columnToRecordToTextWithMaxes mr col
    )
        >> Html.Styled.fromUnstyled
