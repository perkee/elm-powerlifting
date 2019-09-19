module SortColumn exposing (SortColumn(..), compareSavedFeats, fromColumn, toGetter, toString)

-- exposing (SortOrder(..))

import Column exposing (Column)
import Library
import SavedFeat exposing (SavedFeat)
import Scores exposing (featToRecord)


type SortColumn
    = BodyMass
    | LiftedMass
    | Wilks
    | ScaledAllometricIpf
    | ScaledAllometricAtr
    | Allometric
    | IPF
    | McCulloch
    | Index


fromColumn : Column -> Maybe SortColumn
fromColumn col =
    case col of
        Column.BodyKilos ->
            Just BodyMass

        Column.LiftedKilos ->
            Just LiftedMass

        Column.BodyPounds ->
            Just BodyMass

        Column.LiftedPounds ->
            Just LiftedMass

        Column.Wilks ->
            Just Wilks

        Column.ScaledAllometricIpf ->
            Just ScaledAllometricIpf

        Column.ScaledAllometricAtr ->
            Just ScaledAllometricAtr

        Column.Allometric ->
            Just Allometric

        Column.IPF ->
            Just IPF

        Column.McCulloch ->
            Just McCulloch

        Column.Gender ->
            Nothing

        Column.Lift ->
            Nothing

        Column.Equipment ->
            Nothing


toString : SortColumn -> String
toString sc =
    case sc of
        BodyMass ->
            "Body Mass"

        LiftedMass ->
            "Lifted Mass"

        Wilks ->
            "Wilks"

        ScaledAllometricIpf ->
            "Scaled Allometric IPF"

        ScaledAllometricAtr ->
            "Scaled Allometric ATR"

        Allometric ->
            "Allometric"

        IPF ->
            "IPF"

        McCulloch ->
            "McCulloch"

        Index ->
            "Index"


toGetter : SortColumn -> SavedFeat -> Maybe Float
toGetter col =
    case col of
        BodyMass ->
            .feat >> .bodyKilos >> Just

        LiftedMass ->
            .feat >> .liftedKilos >> Just

        Wilks ->
            .feat >> featToRecord >> .wilks

        ScaledAllometricIpf ->
            .feat >> featToRecord >> .scaledAllometricIpf

        ScaledAllometricAtr ->
            .feat >> featToRecord >> .scaledAllometricAtr

        Allometric ->
            .feat >> featToRecord >> .allometric

        IPF ->
            .feat >> featToRecord >> .ipf

        McCulloch ->
            .feat >> featToRecord >> .mcCulloch

        Index ->
            .index >> toFloat >> Just


compareSavedFeats : Library.SortOrder -> SortColumn -> SavedFeat -> SavedFeat -> Order
compareSavedFeats sortOrder sortColumn =
    let
        toDefault =
            Maybe.withDefault <|
                case sortOrder of
                    Library.Ascending ->
                        1 / 0

                    Library.Descending ->
                        -1 / 0
    in
    Library.compose2same
        (toGetter sortColumn >> toDefault)
        (Library.compareByOrder sortOrder)
