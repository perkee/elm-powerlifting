module SavedFeat exposing
    ( IndexedSavedFeat
    , SavedFeat
    , compare
    , maxRecord
    , serialize
    , sortColumnToGetter
    , toList
    )

import Data.Sort as Sort
import Dict exposing (Dict)
import Feat exposing (Feat)
import Json.Encode as E
import Library
import Mass
import Scores
    exposing
        ( Record
        , featToRecord
        )
import SortColumn


type alias SavedFeat =
    { key : Int
    , feat : Feat
    }


type alias IndexedSavedFeat =
    { index : Int
    , savedFeat : SavedFeat
    }


toList : Dict Int SavedFeat -> List IndexedSavedFeat
toList =
    Dict.values >> List.indexedMap ((+) 1 >> IndexedSavedFeat)


serialize : SavedFeat -> E.Value
serialize =
    .feat >> Feat.serialize


maxRecord : List SavedFeat -> Maybe Record
maxRecord =
    List.map (.feat >> featToRecord) >> Scores.maxRecord


compare : Sort.State -> IndexedSavedFeat -> IndexedSavedFeat -> Order
compare state =
    let
        toDefault =
            Maybe.withDefault <|
                case state.sortOrder of
                    Library.Ascending ->
                        1 / 0

                    Library.Descending ->
                        -1 / 0
    in
    Library.compose2same
        (.savedFeat >> sortColumnToGetter state.sortColumn >> toDefault)
        (Library.compareByOrder state.sortOrder)


sortColumnToGetter : SortColumn.SortColumn -> SavedFeat -> Maybe Float
sortColumnToGetter col =
    case col of
        SortColumn.BodyMass ->
            .feat >> .bodyMass >> Mass.toKilos >> Just

        SortColumn.LiftedMass ->
            .feat >> .liftedMass >> Mass.toKilos >> Just

        SortColumn.Wilks ->
            .feat >> featToRecord >> .wilks

        SortColumn.ScaledAllometricIpf ->
            .feat >> featToRecord >> .scaledAllometricIpf

        SortColumn.ScaledAllometricAtr ->
            .feat >> featToRecord >> .scaledAllometricAtr

        SortColumn.Allometric ->
            .feat >> featToRecord >> .allometric

        SortColumn.IPF ->
            .feat >> featToRecord >> .ipf

        SortColumn.McCulloch ->
            .feat >> featToRecord >> .mcCulloch

        SortColumn.Index ->
            -- same as sorting by actual index since keys are monotonic
            .key >> toFloat >> Just
