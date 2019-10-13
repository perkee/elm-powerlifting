port module Main exposing (main)

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
import Feat exposing (Feat, decode, testFeats)
import Html exposing (Html, div, h1, h2, h3, text)
import Html.Attributes exposing (class, style)
import Html.Events as HE
import Html.Styled
import Json.Decode as D
import Json.Encode as E
import Library as L
import LiftForm
import Result
import SavedFeat exposing (SavedFeat)
import View.Cards as Cards
import View.ColumnToggles as ColumnToggles
import View.CurrentTable as CurrentTable
import View.FeatCards as FeatCards
import View.FeatTable as FeatTable


port cache : E.Value -> Cmd msg


port log : E.Value -> Cmd msg


serialize : Model -> E.Value
serialize m =
    E.object
        [ ( "feats", E.list SavedFeat.serialize <| Dict.values m.feats )
        ]


serializeParseError : D.Error -> E.Value
serializeParseError e =
    E.object
        [ ( "issue", E.string "decode JSON" )
        , ( "level", E.string "error" )
        , ( "error", E.string <| D.errorToString e )
        ]


featsToDict : List Feat -> Dict Int SavedFeat
featsToDict =
    List.indexedMap (L.pipe2 SavedFeat (L.toDouble >> Tuple.mapBoth .key identity)) >> Dict.fromList


featsDecoder : D.Decoder (List Feat)
featsDecoder =
    D.field "feats" (D.list Feat.decode)


type alias Flags =
    { cache : String
    , env : String
    }


main : Platform.Program Flags Model Msg
main =
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
    featsToDict testFeats


initFeats : Flags -> ( Dict Int SavedFeat, Cmd Msg )
initFeats flags =
    case
        D.decodeString featsDecoder flags.cache
    of
        Result.Ok loadedFeats ->
            ( featsToDict loadedFeats, Cmd.none )

        Result.Err error ->
            ( if flags.env == "development" then
                someFeats

              else
                Dict.empty
            , serializeParseError error |> log
            )



--Dict.empty


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( feats, cmd ) =
            initFeats flags
    in
    ( { formState = LiftForm.init
      , feats = feats
      , featKey = Dict.size feats
      , featState = ColumnToggles.init initCurrentColumns
      , tableState = ColumnToggles.init initTableColumns
      , deleteConfirmVisibility = Modal.hidden
      , keyToDelete = Nothing
      , cardsState = Cards.init Sort.init
      }
    , cmd
    )


modelToFeat : Model -> Maybe Feat
modelToFeat =
    .formState >> LiftForm.toFeat


type Msg
    = Form LiftForm.Intent
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
    (case msg of
        FeatDisplayUpdated state ->
            { model | featState = state }

        TableDisplayUpdated state ->
            { model | tableState = state }

        Form intent ->
            case intent of
                LiftForm.State state ->
                    { model | formState = state }

                LiftForm.Update state savedFeat ->
                    { model
                        | feats = Dict.insert savedFeat.key savedFeat model.feats
                        , formState = state
                    }

                LiftForm.Create state feat ->
                    { model
                        | feats = Dict.insert model.featKey (SavedFeat model.featKey feat) model.feats
                        , featKey = model.featKey + 1
                        , formState = state
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
                , feats = Dict.remove savedFeat.key model.feats
            }

        DeleteModalAnimated visibility ->
            -- Just fadein
            { model | deleteConfirmVisibility = visibility }

        CardsChanged cardsState ->
            { model | cardsState = cardsState }
    )
        |> L.toDouble
        |> Tuple.mapSecond (serialize >> cache)


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
         , LiftForm.view model.formState Form
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
            ++ Cards.view (SavedFeat.toList model.feats)
                model.tableState
                model.cardsState
                cardMsgs
        )
    , case
        model.feats
            |> SavedFeat.toList
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
