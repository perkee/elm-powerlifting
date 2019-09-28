module SortColumn exposing (SortColumn(..), fromColumn, toComparable, toString)

-- exposing (SortOrder(..))

import Column exposing (Column)


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

-- For Sets or Maps
toComparable : SortColumn -> number
toComparable sc =
    case sc of
        BodyMass ->
            0

        LiftedMass ->
            1

        Wilks ->
            2

        ScaledAllometricIpf ->
            3

        ScaledAllometricAtr ->
            4

        Allometric ->
            5

        IPF ->
            6

        McCulloch ->
            7

        Index ->
            8
