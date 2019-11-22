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
import Browser.Navigation as Nav
import Column
    exposing
        ( initCurrentColumns
        , initTableColumns
        )
import Data.Cards as Cards
import Data.ColumnToggles as ColumnToggles
import Data.Sort as Sort
import Debounce exposing (Debounce)
import Dict exposing (Dict)
import Feat exposing (Feat, decode)
import Html exposing (Html, div, h1, h2, h3, text)
import Html.Attributes exposing (class, style)
import Html.Events as HE
import Html.Styled
import Http
import Json.Decode as D
import Json.Encode as E
import Library as L
import LiftForm
import SavedFeat exposing (SavedFeat)
import Url exposing (Url)
import View.Cards as Cards
import View.ColumnToggles as ColumnToggles
import View.CurrentTable as CurrentTable
import View.FeatCards as FeatCards
import View.FeatTable as FeatTable


port log : E.Value -> Cmd msg


port setPath : String -> Cmd msg


serialize : Model -> E.Value
serialize m =
    [ ( "feats", E.list SavedFeat.serialize <| Dict.values m.feats )
    , ( "version", E.int 0 )
    ]
        |> (case m.parentCacheKey of
                Just p ->
                    (::) ( "parent", E.string p )

                Nothing ->
                    identity
           )
        |> E.object


serverError : String -> Http.Error -> Cmd msg
serverError label error =
    E.object
        [ ( "issue", E.string label )
        , ( "level", E.string "error" )
        , ( "error"
          , (case error of
                Http.BadUrl s ->
                    "BadUrl " ++ s

                Http.Timeout ->
                    "Timeout"

                Http.NetworkError ->
                    "Network Error"

                Http.BadStatus r ->
                    "BadStatus " ++ String.fromInt r

                Http.BadBody badBody ->
                    "BadBody: " ++ badBody
            )
                |> E.string
          )
        ]
        |> log


featsToDict : List Feat -> Dict Int SavedFeat
featsToDict =
    List.indexedMap (L.pipe2 SavedFeat (L.toDouble >> Tuple.mapBoth .key identity)) >> Dict.fromList


featsDecoder : D.Decoder (List Feat)
featsDecoder =
    D.maybe (D.field "version" D.int)
        |> D.andThen versionedFeatsDecoder


versionedFeatsDecoder : Maybe Int -> D.Decoder (List Feat)
versionedFeatsDecoder maybeVersion =
    case Maybe.withDefault 0 maybeVersion of
        0 ->
            D.field "feats" (D.list Feat.decode)

        n ->
            D.fail <|
                "Trying to decode info, but version "
                    ++ String.fromInt n
                    ++ " is not supported."


saveDecoder : D.Decoder String
saveDecoder =
    D.field "key" D.string


noteDebounceConfig : Debounce.Config Msg
noteDebounceConfig =
    { strategy = Debounce.later 1000
    , transform = NoteDebounceMsg
    }


type alias Flags =
    { env : String
    }


type Msg
    = UserClickedLink Browser.UrlRequest
    | BrowserChangedUrl Url
    | ServerRespondedWithCache (Result Http.Error (List Feat))
    | ServerRespondedWithSave (Result Http.Error String)
    | UserChangedForm LiftForm.Intent
    | NoteChanged Int String
    | DeleteModalAnimated Modal.Visibility
    | FeatDisplayUpdated ColumnToggles.State
    | TableDisplayUpdated ColumnToggles.State
    | DeleteButtonClicked Int
    | EditButtonClicked SavedFeat
    | DeleteCanceled
    | DeleteConfirmed
    | CardsChanged Cards.State
    | NoteDebounceMsg Debounce.Msg


main : Platform.Program Flags Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = UserClickedLink
        , onUrlChange = BrowserChangedUrl
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ ColumnToggles.subscriptions model.featState FeatDisplayUpdated
        , ColumnToggles.subscriptions model.tableState TableDisplayUpdated
        , Modal.subscriptions model.deleteConfirmVisibility DeleteModalAnimated
        ]


type alias Model =
    { key : Nav.Key
    , formState : LiftForm.State
    , feats : Dict Int SavedFeat
    , featKey : Int
    , featState : ColumnToggles.State
    , tableState : ColumnToggles.State
    , deleteConfirmVisibility : Modal.Visibility
    , keyToDelete : Maybe Int
    , cardsState : Cards.State
    , parentCacheKey : Maybe String
    , noteDebounce : Debounce E.Value
    }



--Dict.empty


loadCacheFromUrl : Url -> Cmd Msg
loadCacheFromUrl url =
    case url.path |> String.dropLeft 1 of
        "" ->
            Cmd.none

        x ->
            getRemoteCache x


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key
      , formState = LiftForm.init
      , feats = Dict.empty
      , featKey = 0
      , featState = ColumnToggles.init initCurrentColumns
      , tableState = ColumnToggles.init initTableColumns
      , deleteConfirmVisibility = Modal.hidden
      , keyToDelete = Nothing
      , cardsState = Cards.init Sort.init
      , parentCacheKey = Nothing
      , noteDebounce = Debounce.init
      }
    , loadCacheFromUrl url
    )


-- HTTP


getRemoteCache : String -> Cmd Msg
getRemoteCache key =
    Http.get
        { url = "/.netlify/functions/cache?key=" ++ key
        , expect = Http.expectJson ServerRespondedWithCache featsDecoder
        }


saveRemoteCache : E.Value -> Cmd Msg
saveRemoteCache v =
    Http.post
        { url = "/.netlify/functions/cache"
        , body = Http.jsonBody v
        , expect = Http.expectJson ServerRespondedWithSave saveDecoder
        }


modelToFeat : Model -> Maybe Feat
modelToFeat =
    .formState >> LiftForm.toFeat


setNoteOnFeat : String -> Feat -> Feat
setNoteOnFeat note feat =
    { feat | note = note }


setNoteOnSavedFeat : String -> SavedFeat -> SavedFeat
setNoteOnSavedFeat note savedFeat =
    { savedFeat | feat = setNoteOnFeat note savedFeat.feat }


andSave : Model -> ( Model, Cmd Msg )
andSave m =
    ( m, m |> serialize |> saveRemoteCache )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BrowserChangedUrl _ ->
            ( model, Cmd.none )

        UserClickedLink _ ->
            ( model, Cmd.none )

        FeatDisplayUpdated state ->
            ( { model | featState = state }, Cmd.none )

        TableDisplayUpdated state ->
            ( { model | tableState = state }, Cmd.none )

        UserChangedForm intent ->
            case intent of
                LiftForm.State state ->
                    ( { model | formState = state }, Cmd.none )

                LiftForm.Update state savedFeat ->
                    { model
                        | feats = Dict.insert savedFeat.key savedFeat model.feats
                        , formState = state
                    }
                        |> andSave

                LiftForm.Create state feat ->
                    { model
                        | feats = Dict.insert model.featKey (SavedFeat model.featKey feat) model.feats
                        , featKey = model.featKey + 1
                        , formState = state
                    }
                        |> andSave

        NoteChanged key note ->
            let
                feats =
                    Dict.update key (Maybe.map <| setNoteOnSavedFeat note) model.feats

                newModel =
                    { model
                        | feats = Dict.update key (Maybe.map <| setNoteOnSavedFeat note) model.feats
                    }

                ( debounce, cmd ) =
                    Debounce.push noteDebounceConfig (newModel |> serialize) model.noteDebounce
            in
            ( { newModel | noteDebounce = debounce }, cmd )

        DeleteCanceled ->
            ( { model
                | deleteConfirmVisibility = Modal.hidden
                , keyToDelete = Nothing
              }
            , Cmd.none
            )

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
                |> andSave

        DeleteButtonClicked key ->
            ( { model
                | deleteConfirmVisibility = Modal.shown
                , keyToDelete = Just key
              }
            , Cmd.none
            )

        EditButtonClicked savedFeat ->
            ( { model
                | formState =
                    LiftForm.pushSavedFeat
                        model.formState
                        savedFeat
                , feats = Dict.remove savedFeat.key model.feats
              }
            , Cmd.none
            )

        DeleteModalAnimated visibility ->
            -- Just fadein
            ( { model | deleteConfirmVisibility = visibility }, Cmd.none )

        CardsChanged cardsState ->
            ( { model | cardsState = cardsState }, Cmd.none )

        ServerRespondedWithCache result ->
            case result of
                Ok feats ->
                    ( { model | feats = featsToDict feats }, Cmd.none )

                Err e ->
                    ( model, serverError "ServerRespondedWithCache" e )

        ServerRespondedWithSave result ->
            case result of
                Ok key ->
                    ( { model | parentCacheKey = Just key }, setPath key )

                Err e ->
                    ( model, serverError "ServerRespondedWithSave" e )

        NoteDebounceMsg debounceMsg ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        noteDebounceConfig
                        (Debounce.takeLast saveRemoteCache)
                        debounceMsg
                        model.noteDebounce
            in
            ( { model | noteDebounce = debounce }
            , cmd
            )


cardMsgs : FeatCards.CardMsgs Msg
cardMsgs =
    FeatCards.CardMsgs
        CardsChanged
        NoteChanged
        DeleteButtonClicked
        EditButtonClicked


view : Model -> Browser.Document Msg
view model =
    { title = "Powerlifting score calculator by @perk.ee"
    , body = body model
    }


body : Model -> List (Html Msg)
body model =
    [ Grid.container []
        ([ h1 [ Html.Attributes.attribute "data-test" "title" ] [ text "Every Score Calculator" ]
         , LiftForm.view model.formState UserChangedForm
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
