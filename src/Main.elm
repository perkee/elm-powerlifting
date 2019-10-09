module Main exposing (main)

-- (Html, button, div, text, input, option, select)
-- imports used

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
import Dict exposing (Dict)
import Feat exposing (Feat, testFeats)
import Html exposing (Html, div, h1, h2, h3, text)
import Html.Attributes exposing (class, style)
import Html.Events as HE
import Html.Styled
import Library as L exposing (SortOrder(..))
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
    , feats : Dict Int SavedFeat
    , featKey : Int
    , featState : ColumnToggles.State
    , tableState : ColumnToggles.State
    , deleteConfirmVisibility : Modal.Visibility
    , keyToDelete : Maybe Int
    , cardsState : Cards.State
    }


someFeats : Dict Int SavedFeat
someFeats =
    testFeats
        |> List.map2 (\key feat -> ( key, SavedFeat key feat ))
            [ 1
            , 2
            , 3
            ]
        |> Dict.fromList


init : String -> ( Model, Cmd Msg )
init nodeEnv =
    ( { formState = LiftForm.init
      , feats =
            if nodeEnv == "development" then
                --Array.empty
                someFeats

            else
                Dict.empty
      , featKey =
            if nodeEnv == "development" then
                4

            else
                0
      , featState = ColumnToggles.init initCurrentColumns
      , tableState = ColumnToggles.init initTableColumns
      , deleteConfirmVisibility = Modal.hidden
      , keyToDelete = Nothing
      , cardsState = Cards.init Sort.init
      }
    , Cmd.none
    )


modelToFeat : Model -> Maybe Feat
modelToFeat =
    .formState >> LiftForm.toFeat


type Msg
    = FormUpdated LiftForm.State
    | SaveButtonClicked Feat
    | UpdateButtonClicked SavedFeat
    | DiscardButtonClicked
    | NoteChanged Int String
    | DeleteModalAnimated Modal.Visibility
    | FeatDisplayUpdated ColumnToggles.State
    | TableDisplayUpdated ColumnToggles.State
    | DeleteButtonClicked Int
    | EditButtonClicked SavedFeat
    | DeleteCanceled
    | DeleteConfirmed
    | CardsChanged Cards.State


liftFormMessages : LiftForm.Messages Msg
liftFormMessages =
    { state = FormUpdated
    , createRow = SaveButtonClicked
    , updateRow = UpdateButtonClicked
    , discard = DiscardButtonClicked
    }


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

        SaveButtonClicked feat ->
            { model
                | feats = Dict.insert model.featKey (SavedFeat model.featKey feat) model.feats
                , featKey = model.featKey + 1
                , formState = LiftForm.popState model.formState
            }

        UpdateButtonClicked savedFeat ->
            { model
                | feats = Dict.insert savedFeat.key savedFeat model.feats
                , formState = LiftForm.popState model.formState
            }

        DiscardButtonClicked ->
            { model
                | formState = LiftForm.popState model.formState
            }

        NoteChanged key note ->
            { model
                | feats = Dict.update key (Maybe.map <| setNoteOnSavedFeat note) model.feats
            }

        DeleteCanceled ->
            { model
                | deleteConfirmVisibility = Modal.hidden
                , keyToDelete = Nothing
            }

        DeleteConfirmed ->
            { model
                | deleteConfirmVisibility = Modal.hidden
                , feats =
                    case model.keyToDelete of
                        Just key ->
                            Dict.remove key model.feats

                        Nothing ->
                            model.feats
                , keyToDelete = Nothing
            }

        DeleteButtonClicked key ->
            { model
                | deleteConfirmVisibility = Modal.shown
                , keyToDelete = Just key
            }

        EditButtonClicked savedFeat ->
            { model
                | formState =
                    LiftForm.pushSavedFeat
                        model.formState
                        savedFeat
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
         , LiftForm.view model.formState liftFormMessages
         , h2 [] [ text "Current Score" ]
         , Grid.row [ Row.attrs [ class "current-table" ] ]
            [ Grid.col [ Col.xs12 ]
                (case modelToFeat model of
                    Just feat ->
                        [ ColumnToggles.config FeatDisplayUpdated "current-column-toggles"
                            |> ColumnToggles.title "Current Scores Options"
                            |> ColumnToggles.view model.featState
                        , CurrentTable.view
                            (Dict.values model.feats)
                            (ColumnToggles.columns model.featState)
                            feat
                            |> Html.Styled.toUnstyled
                        ]

                    Nothing ->
                        [ Alert.simpleInfo [] [ text "Enter data to see all scores for a lift" ] ]
                )
            ]
         , h2 [] [ text "Scores Table" ]
         , if Dict.isEmpty model.feats then
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
            ++ Cards.view (Dict.values model.feats)
                model.tableState
                model.cardsState
                cardMsgs
        )
    , case
        model.feats
            |> Dict.values
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
                        [ (case model.keyToDelete of
                            Just key ->
                                "Are you sure you want to delete the entry at row "
                                    ++ String.fromInt key

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
