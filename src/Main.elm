module Main exposing (main)

-- (Html, button, div, text, input, option, select)
-- imports used

import Array exposing (Array)
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import Browser
import Column
    exposing
        ( initCurrentColumns
        , initTableColumns
        )
import Data.Cards as Cards
import Data.ColumnToggles as ColumnToggles
import Data.Sort as Sort
import Feat exposing (Feat, testFeats)
import Html exposing (Html, div, h1, h2, h3, text)
import Html.Attributes exposing (class, style)
import Html.Events as HE
import Html.Styled
import Library exposing (SortOrder(..), removeAt, updateArrayAt)
import LiftForm
import SavedFeat exposing (SavedFeat)
import View.Cards as Cards
import View.ColumnToggles as ColumnToggles
import View.CurrentTable as CurrentTable
import View.FeatCards as FeatCards
import View.FeatTable as FeatTable


main : Platform.Program String Model Msg
main =
    -- Flags is only one field, so type is String.
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ ColumnToggles.subscriptions model.featState FeatDisplayUpdated
        , ColumnToggles.subscriptions model.tableState TableDisplayUpdated
        , Modal.subscriptions model.deleteConfirmVisibility DeleteModalAnimated
        ]


type alias Model =
    { formState : LiftForm.State
    , feats : Array SavedFeat
    , featKey : Int
    , featState : ColumnToggles.State
    , tableState : ColumnToggles.State
    , deleteConfirmVisibility : Modal.Visibility
    , idxToDelete : Maybe Int
    , cardsState : Cards.State
    }


someFeats : Array SavedFeat
someFeats =
    testFeats
        |> List.map2 (\( index, key ) feat -> SavedFeat feat index key)
            [ ( 0, 2 )
            , ( 1, 2 )
            , ( 2, 2 )
            ]
        |> Array.fromList


init : String -> ( Model, Cmd Msg )
init nodeEnv =
    ( { formState = LiftForm.init
      , feats =
            if nodeEnv == "development" then
                --Array.empty
                someFeats

            else
                Array.empty
      , featKey = 0
      , featState = ColumnToggles.init initCurrentColumns
      , tableState = ColumnToggles.init initTableColumns
      , deleteConfirmVisibility = Modal.hidden
      , idxToDelete = Nothing
      , cardsState = Cards.init Sort.init
      }
    , Cmd.none
    )


modelToFeat : Model -> Maybe Feat
modelToFeat =
    .formState >> LiftForm.toFeat


type Msg
    = FormUpdated LiftForm.State
    | SaveButtonClicked (Maybe Feat)
    | NoteChanged Int String
    | DeleteModalAnimated Modal.Visibility
    | FeatDisplayUpdated ColumnToggles.State
    | TableDisplayUpdated ColumnToggles.State
    | DeleteButtonClicked Int
    | EditButtonClicked SavedFeat
    | DeleteCanceled
    | DeleteConfirmed
    | CardsChanged Cards.State


setNoteOnFeat : String -> Feat -> Feat
setNoteOnFeat note feat =
    { feat | note = note }


setNoteOnSavedFeat : String -> SavedFeat -> SavedFeat
setNoteOnSavedFeat note savedFeat =
    { savedFeat | feat = setNoteOnFeat note savedFeat.feat }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        FeatDisplayUpdated state ->
            { model | featState = state }

        TableDisplayUpdated state ->
            { model | tableState = state }

        FormUpdated state ->
            { model | formState = state }

        SaveButtonClicked mf ->
            case mf of
                Just feat ->
                    { model
                        | feats = Array.push (SavedFeat feat (Array.length model.feats) model.featKey) model.feats
                        , featKey = model.featKey + 1
                    }

                Nothing ->
                    model

        NoteChanged index note ->
            { model | feats = updateArrayAt index (setNoteOnSavedFeat note) model.feats }

        DeleteCanceled ->
            { model
                | deleteConfirmVisibility = Modal.hidden
                , idxToDelete = Nothing
            }

        DeleteConfirmed ->
            { model
                | deleteConfirmVisibility = Modal.hidden
                , feats =
                    case model.idxToDelete of
                        Just idx ->
                            model.feats
                                |> removeAt idx
                                |> Array.indexedMap
                                    (\i f ->
                                        if i < idx then
                                            f

                                        else
                                            { f | index = f.index - 1 }
                                    )

                        Nothing ->
                            model.feats
                , idxToDelete = Nothing
            }

        DeleteButtonClicked idx ->
            { model
                | deleteConfirmVisibility = Modal.shown
                , idxToDelete = Just idx
            }

        EditButtonClicked savedFeat ->
            { model
                | formState = LiftForm.fromFeat savedFeat.feat
            }

        DeleteModalAnimated visibility ->
            -- Just fadein
            { model | deleteConfirmVisibility = visibility }

        CardsChanged cardsState ->
            { model | cardsState = cardsState }
    , Cmd.none
    )


cardMsgs : FeatCards.CardMsgs Msg
cardMsgs =
    FeatCards.CardMsgs
        CardsChanged
        NoteChanged
        DeleteButtonClicked
        EditButtonClicked


view : Model -> Html Msg
view model =
    [ Grid.container []
        ([ h1 [] [ text "Every Score Calculator" ]
         , LiftForm.view model.formState FormUpdated <| SaveButtonClicked <| modelToFeat <| model
         , h2 [] [ text "Current Score" ]
         , Grid.row [ Row.attrs [ class "current-table" ] ]
            [ Grid.col [ Col.xs12 ]
                (case modelToFeat model of
                    Just feat ->
                        [ ColumnToggles.config FeatDisplayUpdated "current-column-toggles"
                            |> ColumnToggles.title "Current Scores Options"
                            |> ColumnToggles.view model.featState
                        , CurrentTable.view
                            (Array.toList model.feats)
                            (ColumnToggles.columns model.featState)
                            feat
                            |> Html.Styled.toUnstyled
                        ]

                    Nothing ->
                        [ Alert.simpleInfo [] [ text "Enter data to see all scores for a lift" ] ]
                )
            ]
         , h2 [] [ text "Scores Table" ]
         , if Array.isEmpty model.feats then
            Alert.simpleInfo [] [ text "Add scores to the table to compare" ]

           else
            ColumnToggles.config TableDisplayUpdated "table-column-toggles"
                |> ColumnToggles.title "Table Options"
                |> ColumnToggles.view model.tableState
                |> List.singleton
                |> Grid.col [ Col.xs12 ]
                |> List.singleton
                |> Grid.row [ Row.attrs [ style "margin-bottom" ".75rem" ] ]
         ]
            ++ Cards.view (Array.toList model.feats)
                model.tableState
                model.cardsState
                cardMsgs
        )
    , case
        model.feats
            |> Array.toList
            |> FeatTable.view
                model.tableState
                model.cardsState
                cardMsgs
      of
        Just table ->
            Grid.containerFluid [ class "d-none d-md-block" ]
                [ Grid.row []
                    [ Grid.col [ Col.sm12 ]
                        [ table |> Html.Styled.toUnstyled ]
                    ]
                ]

        Nothing ->
            text ""
    , Modal.config DeleteCanceled
        |> Modal.withAnimation DeleteModalAnimated
        |> Modal.large
        |> Modal.h3 [] [ text "Confirm delete" ]
        |> Modal.body []
            [ Grid.containerFluid []
                [ Grid.row []
                    [ Grid.col
                        [ Col.xs12 ]
                        [ (case model.idxToDelete of
                            Just idx ->
                                "Are you sure you want to delete the entry at row "
                                    ++ String.fromInt idx

                            Nothing ->
                                ""
                          )
                            |> text
                        ]
                    ]
                ]
            ]
        |> Modal.footer []
            [ Button.button
                [ Button.outlineDanger

                -- cannot currently animate this on confirm. Gets into an infinite loop rn. Kinda impresseive!
                , Button.attrs [ HE.onClick <| DeleteConfirmed ]
                ]
                [ text "Yes, Delete this row" ]
            , Button.button
                [ Button.outlineSuccess
                , Button.attrs [ HE.onClick <| DeleteCanceled ]
                ]
                [ text "Nevermind" ]
            ]
        |> Modal.view model.deleteConfirmVisibility
    ]
        |> div []
