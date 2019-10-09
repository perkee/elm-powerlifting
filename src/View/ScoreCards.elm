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
import Data.Cards as Cards
import Data.ColumnToggles as ColumnToggles
import Data.Sort as Sort
import Feat exposing (Feat, liftToLetter)
import Html exposing (Html, text)
import Html.Attributes exposing (class)
import Html.Styled
import Html.Styled.Attributes as HSA
import Html.Styled.Keyed
import Library exposing (snocnu)
import Mass exposing (MassUnit)
import Renderer exposing (floatToString, rowsToHeadedTable, styledIcon)
import SavedFeat exposing (SavedFeat)
import Scores
    exposing
        ( featToRecord
        , maybeMax
        )
import Set
import SortColumn exposing (SortColumn(..))
import View.FeatCards as FeatCards


type alias NoteChangedMsg msg =
    Int -> String -> msg


type alias State =
    { summaryUnit : MassUnit
    , togglesState : ColumnToggles.State
    }


init : ColumnToggles.State -> State
init =
    State Mass.KG


view :
    List SavedFeat
    -> State
    -> Cards.State
    -> FeatCards.CardMsgs msg
    -> Html msg
view savedFeats state cardsState cardMsgs =
    Grid.row []
        [ savedFeats
            |> savedFeatsToLiftCards
                (ColumnToggles.columns state.togglesState)
                state.summaryUnit
                (Cards.toggleMassUnit cardsState |> cardMsgs.cardsChanged)
                cardMsgs.noteChanged
            |> Card.keyedColumns
            |> List.singleton
            |> Grid.col [ Col.xs12, Col.attrs [ class "d-md-none" ] ]
        ]


savedFeatsToLiftCards : List Column -> MassUnit -> msg -> NoteChangedMsg msg -> List SavedFeat -> List ( String, Card.Config msg )
savedFeatsToLiftCards cols massUnit massUnitMsg noteChangedMsg savedFeats =
    cols
        |> List.foldr
            (\col ( acc, haves ) ->
                case SortColumn.fromColumn col of
                    Just sc ->
                        let
                            scName =
                                SortColumn.toComparable sc
                        in
                        if Set.member scName haves then
                            ( acc, haves )

                        else
                            ( ( col, sc ) :: acc, Set.insert scName haves )

                    Nothing ->
                        ( acc, haves )
            )
            ( [], Set.empty )
        |> Tuple.first
        |> List.map (columnToLiftCard savedFeats massUnit massUnitMsg noteChangedMsg)


columnToLiftCard : List SavedFeat -> MassUnit -> msg -> NoteChangedMsg msg -> ( Column, SortColumn.SortColumn ) -> ( String, Card.Config msg )
columnToLiftCard savedFeats liftCardsUnit massUnitMsg noteChangedMsg ( col, sortCol ) =
    let
        label =
            sortCol |> SortColumn.toString

        max =
            savedFeats
                |> List.map (SavedFeat.sortColumnToGetter sortCol)
                |> List.foldl maybeMax (Just (-1 / 0))

        rows =
            Sort.State sortCol Library.Descending
                |> SavedFeat.compare
                |> List.sortWith
                |> Library.thrush savedFeats
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
                Html.Styled.toUnstyled <|
                    rowsToHeadedTable
                        ([ ( "", Html.Styled.text "" )
                         , ( "Note", Html.Styled.text "" )
                         ]
                            |> (if showLift savedFeats then
                                    snocnu ( "Lift", Html.Styled.text "" )

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
                                            Mass.KG ->
                                                "Kg"

                                            Mass.LBM ->
                                                "Lb."
                                    ]
                                    |> Html.Styled.fromUnstyled
                                )
                            |> (if sortCol == SortColumn.LiftedMass || sortCol == SortColumn.BodyMass then
                                    identity

                                else
                                    snocnu ( "Value", Html.Styled.text "" )
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


savedFeatToRow : NoteChangedMsg msg -> ( Column, SortColumn.SortColumn ) -> Bool -> MassUnit -> Maybe Float -> SavedFeat -> ( String, Html.Styled.Html msg )
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
            , case ( maybeMaximum, SavedFeat.sortColumnToGetter sortCol savedFeat ) of
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
                [ savedFeat.key |> String.fromInt |> Html.Styled.text ]
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
    )


summaryCell : MassUnit -> Feat -> Html.Styled.Html msg
summaryCell massUnit feat =
    let
        ( featToSummary, classModifier ) =
            case massUnit of
                Mass.LBM ->
                    ( featToSummaryPounds, "lbm" )

                Mass.KG ->
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
        , Input.onInput <| noteChangedMsg savedFeat.key
        , Input.attrs [ class <| "note-input note-input--" ++ classSuffix ]
        ]


featToSummaryPounds : Feat -> String
featToSummaryPounds f =
    (f.liftedMass |> Mass.toPounds |> floatToString)
        ++ " @ "
        ++ (f.bodyMass |> Mass.toPounds |> floatToString)


featToSummaryKilos : Feat -> String
featToSummaryKilos f =
    (f.liftedMass |> Mass.toKilos |> floatToString)
        ++ " @ "
        ++ (f.bodyMass |> Mass.toKilos |> floatToString)
