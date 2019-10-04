module Column exposing
    ( Column(..)
    , allColumns
    , columnToColumnLabel
    , columnToRecordToText
    , columnToRecordToTextWithMaxes
    , columnToToggleLabel
    , initCurrentColumns
    , initTableColumns
    )

import Bootstrap.Progress as Progress
import Feat exposing (equipmentToString, genderToString, liftToString)
import Html as H exposing (Html)
import Mass
import Renderer exposing (floatToString, maybeFloatToString)
import Scores exposing (Record)


type Column
    = BodyKilos
    | LiftedKilos
    | BodyPounds
    | LiftedPounds
    | Wilks
    | ScaledAllometricIpf
    | ScaledAllometricAtr
    | Allometric
    | IPF
    | McCulloch
    | Gender
    | Lift
    | Equipment


allColumns : List Column
allColumns =
    [ Gender
    , Lift
    , Equipment
    , LiftedKilos
    , BodyKilos
    , LiftedPounds
    , BodyPounds
    , Wilks
    , Allometric
    , IPF
    , McCulloch
    , ScaledAllometricIpf
    , ScaledAllometricAtr
    ]


initCurrentColumns : List Column
initCurrentColumns =
    [ Gender
    , Lift
    , Equipment
    , LiftedKilos
    , BodyKilos
    , LiftedPounds
    , BodyPounds
    , Wilks
    , Allometric
    , IPF
    , McCulloch
    , ScaledAllometricAtr
    , ScaledAllometricIpf
    ]


initTableColumns : List Column
initTableColumns =
    [ Gender
    , Lift
    , Equipment
    , LiftedKilos
    , BodyKilos
    , Wilks
    , IPF
    , ScaledAllometricIpf
    , Allometric
    ]


unitSeparatorSpace : String
unitSeparatorSpace =
    String.fromChar '\u{200A}'


columnToColumnLabel : Column -> String
columnToColumnLabel column =
    case column of
        BodyKilos ->
            "BW" ++ unitSeparatorSpace ++ "(kg)"

        BodyPounds ->
            "BW" ++ unitSeparatorSpace ++ "(lb)"

        ScaledAllometricIpf ->
            "Sc. Allo. IPF"

        ScaledAllometricAtr ->
            "Sc. Allo. ATR"

        Allometric ->
            "Allo."

        McCulloch ->
            "McC–W"

        x ->
            columnToToggleLabel x


columnToToggleLabel : Column -> String
columnToToggleLabel column =
    case column of
        Gender ->
            "Gender"

        Lift ->
            "Lift"

        Equipment ->
            "Equip."

        LiftedKilos ->
            "Lift" ++ unitSeparatorSpace ++ "(kg)"

        BodyKilos ->
            "Bodyweight" ++ unitSeparatorSpace ++ "(kg)"

        LiftedPounds ->
            "Lift" ++ unitSeparatorSpace ++ "(lb)"

        BodyPounds ->
            "Bodyweight" ++ unitSeparatorSpace ++ "(lb)"

        Wilks ->
            "Wilks"

        ScaledAllometricIpf ->
            "Scaled Allometric (IPF)"

        ScaledAllometricAtr ->
            "Scaled Allometric (All Time Raw w/ Wraps)"

        Allometric ->
            "Allometric"

        IPF ->
            "IPF"

        McCulloch ->
            "McCulloch–Wilks"


floatToProgress : Float -> Float -> Html msg
floatToProgress max val =
    Progress.progress
        [ (val / max * 100) |> Progress.value
        , val |> floatToString |> Progress.label
        ]


maybeFloatToProgress : Maybe Float -> Maybe Float -> Html msg
maybeFloatToProgress maybeMax maybeVal =
    case ( maybeMax, maybeVal ) of
        ( Just max, Just val ) ->
            floatToProgress max val

        ( _, anyVal ) ->
            maybeFloatToString anyVal |> H.text


columnToRecordToTextWithMaxes : Record -> Column -> Record -> Html msg
columnToRecordToTextWithMaxes maxes column =
    case column of
        Gender ->
            .feat >> .gender >> genderToString >> H.text

        Lift ->
            .feat >> .lift >> liftToString >> H.text

        Equipment ->
            .feat >> .equipment >> equipmentToString >> H.text

        LiftedKilos ->
            .feat >> .liftedMass >> Mass.toKilos >> floatToProgress (Mass.toKilos maxes.feat.liftedMass)

        BodyKilos ->
            .feat >> .bodyMass >> Mass.toKilos >> floatToProgress (Mass.toKilos maxes.feat.bodyMass)

        LiftedPounds ->
            .feat >> .liftedMass >> Mass.toPounds >> floatToProgress (Mass.toPounds maxes.feat.liftedMass)

        BodyPounds ->
            .feat >> .bodyMass >> Mass.toPounds >> floatToProgress (Mass.toPounds maxes.feat.bodyMass)

        Wilks ->
            .wilks >> maybeFloatToProgress maxes.wilks

        ScaledAllometricIpf ->
            .scaledAllometricIpf >> maybeFloatToProgress maxes.scaledAllometricIpf

        ScaledAllometricAtr ->
            .scaledAllometricAtr >> maybeFloatToProgress maxes.scaledAllometricAtr

        Allometric ->
            .allometric >> maybeFloatToProgress maxes.allometric

        IPF ->
            .ipf >> maybeFloatToProgress maxes.ipf

        McCulloch ->
            .mcCulloch >> maybeFloatToProgress maxes.mcCulloch


columnToRecordToText : Column -> Record -> Html msg
columnToRecordToText column =
    (<<) H.text <|
        case column of
            Gender ->
                .feat >> .gender >> genderToString

            Lift ->
                .feat >> .lift >> liftToString

            Equipment ->
                .feat >> .equipment >> equipmentToString

            LiftedKilos ->
                .feat >> .liftedMass >> Mass.toKilos >> floatToString

            BodyKilos ->
                .feat >> .bodyMass >> Mass.toKilos >> floatToString

            LiftedPounds ->
                .feat >> .liftedMass >> Mass.toPounds >> floatToString

            BodyPounds ->
                .feat >> .bodyMass >> Mass.toPounds >> floatToString

            Wilks ->
                .wilks >> maybeFloatToString

            ScaledAllometricIpf ->
                .scaledAllometricIpf >> maybeFloatToString

            ScaledAllometricAtr ->
                .scaledAllometricAtr >> maybeFloatToString

            Allometric ->
                .allometric >> maybeFloatToString

            IPF ->
                .ipf >> maybeFloatToString

            McCulloch ->
                .mcCulloch >> maybeFloatToString
