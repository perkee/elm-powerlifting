module SavedFeat exposing
    ( SavedFeat
    , compare
    , maxRecord
    , sortColumnToGetter
    )

import Data.Sort as Sort
import Feat exposing (Feat)
import Library
import Scores
    exposing
        ( Record
        , featToRecord
        )
import SortColumn


type alias SavedFeat =
    { feat : Feat
    , index : Int
    , key : Int
    }


maxRecord : List SavedFeat -> Record
maxRecord =
    List.map (.feat >> featToRecord) >> Scores.maxRecord


compare : Sort.State -> SavedFeat -> SavedFeat -> Order
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
        (sortColumnToGetter state.sortColumn >> toDefault)
        (Library.compareByOrder state.sortOrder)


sortColumnToGetter : SortColumn.SortColumn -> SavedFeat -> Maybe Float
sortColumnToGetter col =
    case col of
        SortColumn.BodyMass ->
            .feat >> .bodyKilos >> Just

        SortColumn.LiftedMass ->
            .feat >> .liftedKilos >> Just

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
            .index >> toFloat >> Just
