module View.ScoreCards exposing (State, init, view)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Column
    exposing
        ( Column
        )
import Css
import Data.ColumnToggles as ColumnToggles
import Feat exposing (Feat, MassUnit, liftToLetter)
import Html exposing (Html, h3, text)
import Html.Attributes exposing (class)
import Html.Styled
import Html.Styled.Attributes as HSA
import Html.Styled.Keyed
import Library exposing (snocnu)
import Renderer exposing (floatToString, rowsToHeadedTable, styledIcon)
import SavedFeat exposing (SavedFeat)
import Scores
    exposing
        ( featToRecord
        , maybeMax
        )
import SortColumn exposing (SortColumn(..))


type alias NoteChangedMsg msg =
    Int -> String -> msg


type alias State =
    { summaryUnit : MassUnit
    , togglesState : ColumnToggles.State
    }


init : ColumnToggles.State -> State
init =
    State Feat.KG


view : List SavedFeat -> State -> msg -> NoteChangedMsg msg -> List (Html msg)
view savedFeats state massUnitMsg noteChangedMsg =
    [ h3 [ class "d-md-none" ] [ text "Grouped by Score" ]
    , Grid.row []
        [ savedFeats
            |> savedFeatsToLiftCards
                (ColumnToggles.columns state.togglesState)
                state.summaryUnit
                massUnitMsg
                noteChangedMsg
            |> Card.keyedColumns
            |> List.singleton
            |> Grid.col [ Col.xs12, Col.attrs [ class "d-md-none" ] ]
        ]
    ]


savedFeatsToLiftCards : List Column -> MassUnit -> msg -> NoteChangedMsg msg -> List SavedFeat -> List ( String, Card.Config msg )
savedFeatsToLiftCards cols massUnit massUnitMsg noteChangedMsg savedFeats =
    cols
        |> List.foldr
            (\col acc ->
                case SortColumn.fromColumn col of
                    Just sc ->
                        ( col, sc ) :: acc

                    Nothing ->
                        acc
            )
            []
        |> List.map (columnToLiftCard savedFeats massUnit massUnitMsg noteChangedMsg)


columnToLiftCard : List SavedFeat -> MassUnit -> msg -> NoteChangedMsg msg -> ( Column, SortColumn.SortColumn ) -> ( String, Card.Config msg )
columnToLiftCard savedFeats liftCardsUnit massUnitMsg noteChangedMsg ( col, sortCol ) =
    let
        label =
            sortCol |> SortColumn.toString

        max =
            savedFeats
                |> List.map (SortColumn.toGetter sortCol)
                |> List.foldl maybeMax (Just (-1 / 0))

        rows =
            savedFeats
                |> List.sortWith (SortColumn.compareSavedFeats Library.Descending sortCol)
                |> List.map
                    (savedFeatToRow
                        noteChangedMsg
                        ( col, sortCol )
                        (showLift savedFeats)
                        liftCardsUnit
                        max
                    )
    in
    ( label
    , Card.config
        [ Card.attrs [ class "lift-card" ]
        ]
        |> Card.headerH4 []
            [ text label
            ]
        |> Card.block [ Block.attrs [ class "lift-card__block" ] ]
            [ Block.custom <|
                rowsToHeadedTable
                    ([ ( "", text "" )
                     , ( "Note", text "" )
                     ]
                        |> (if showLift savedFeats then
                                snocnu ( "Lift", text "" )

                            else
                                identity
                           )
                        |> snocnu
                            ( ""
                            , Button.button
                                [ Button.outlineSecondary
                                , Button.onClick massUnitMsg
                                , Button.small
                                , Button.block
                                ]
                                [ text <|
                                    case liftCardsUnit of
                                        Feat.KG ->
                                            "Kg"

                                        Feat.LBM ->
                                            "Lb."
                                ]
                            )
                        |> (if sortCol == SortColumn.LiftedMass || sortCol == SortColumn.BodyMass then
                                identity

                            else
                                snocnu ( "Value", text "" )
                           )
                    )
                    rows
            ]
    )


showLift : List SavedFeat -> Bool
showLift savedFeats =
    case savedFeats of
        [] ->
            False

        _ :: [] ->
            False

        feat :: feats ->
            feats |> List.foldl (\f anyDiff -> anyDiff || (f.feat |> .lift) /= (feat.feat |> .lift)) False


liftCardCell : String -> List (Html.Styled.Html msg) -> Html.Styled.Html msg
liftCardCell className =
    Html.Styled.td
        [ HSA.class className
        ]


savedFeatToRow : NoteChangedMsg msg -> ( Column, SortColumn.SortColumn ) -> Bool -> MassUnit -> Maybe Float -> SavedFeat -> ( String, Html msg )
savedFeatToRow noteChangedMsg ( col, sortCol ) shouldShowLift liftCardsUnit maybeMaximum savedFeat =
    let
        key =
            String.fromInt savedFeat.key ++ (sortCol |> SortColumn.toString)

        record =
            featToRecord savedFeat.feat

        barColor =
            Css.rgba 0 123 255 0.2

        pct =
            \val max -> Css.pct <| (val / max * 100)
    in
    ( key
    , Html.Styled.Keyed.node "tr"
        [ HSA.class "lift-card__data-row"
        , HSA.css
            [ Css.backgroundAttachment Css.fixed
            , case ( maybeMaximum, SortColumn.toGetter sortCol savedFeat ) of
                ( Just max, Just val ) ->
                    Css.backgroundImage <|
                        Css.linearGradient2
                            Css.toRight
                            (Css.stop barColor)
                            (Css.stop2 barColor (pct val max))
                            [ Css.stop2 (Css.rgba 0 0 0 0) (pct val max)
                            , Css.stop (Css.rgba 0 0 0 0)
                            ]

                ( _, _ ) ->
                    Css.backgroundImage <| Css.none
            ]
        ]
        ([ ( key ++ "idx"
           , Html.Styled.th
                [ HSA.class "index"
                ]
                [ savedFeat.index |> String.fromInt |> Html.Styled.text ]
           )
         , ( key ++ "note", liftCardCell "note" [ savedFeatToNoteInput "lift-card" savedFeat noteChangedMsg |> Html.Styled.fromUnstyled ] )
         ]
            |> (if shouldShowLift then
                    snocnu ( key ++ "lift", liftCardCell "lift" [ savedFeat.feat |> .lift |> liftToLetter |> Html.Styled.text ] )

                else
                    identity
               )
            |> snocnu ( key ++ "summary", summaryCell liftCardsUnit savedFeat.feat )
            |> (if sortCol == SortColumn.LiftedMass || sortCol == SortColumn.BodyMass then
                    identity

                else
                    snocnu ( key ++ "score", liftCardCell "value" [ record |> Column.columnToRecordToText col |> Html.Styled.fromUnstyled ] )
               )
        )
        |> Html.Styled.toUnstyled
    )


summaryCell : MassUnit -> Feat -> Html.Styled.Html msg
summaryCell massUnit feat =
    let
        ( featToSummary, classModifier ) =
            case massUnit of
                Feat.LBM ->
                    ( featToSummaryPounds, "lbm" )

                Feat.KG ->
                    ( featToSummaryKilos, "kg" )

        genderIcon =
            styledIcon <|
                case feat.gender of
                    Feat.Male ->
                        "mars"

                    Feat.Female ->
                        "venus"

                    Feat.GNC ->
                        "genderless"
    in
    Html.Styled.td
        [ HSA.class <| "summary summary--" ++ classModifier
        , HSA.css
            [ Css.minWidth <| Css.ex <| Library.phi + 11.1
            ]
        ]
        [ genderIcon
            [ HSA.css
                [ Css.marginRight <| Css.ex <| Library.phi - 1
                , Css.width <| Css.ex 1
                ]
            ]
        , featToSummary feat |> Html.Styled.text
        ]


savedFeatToNoteInput : String -> SavedFeat -> NoteChangedMsg msg -> Html msg
savedFeatToNoteInput classSuffix savedFeat noteChangedMsg =
    Input.text
        [ Input.placeholder "Note"
        , Input.value <| .note <| savedFeat.feat
        , Input.onInput <| noteChangedMsg savedFeat.index
        , Input.attrs [ class <| "note-input note-input--" ++ classSuffix ]
        ]


featToSummaryPounds : Feat -> String
featToSummaryPounds f =
    floatToString f.liftedPounds ++ " @ " ++ floatToString f.bodyPounds


featToSummaryKilos : Feat -> String
featToSummaryKilos f =
    floatToString f.liftedKilos ++ " @ " ++ floatToString f.bodyKilos