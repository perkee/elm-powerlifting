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
import Feat exposing (genderToString, liftToString)
import Html as H exposing (Html)
import Html.Attributes as HA
import Library exposing (thrush)
import Renderer exposing (floatToString, maybeFloatToString, rowsToHeadedTable)
import Scores exposing (Record)


type Column
    = BodyKilos
    | LiftedKilos
    | BodyPounds
    | LiftedPounds
    | Wilks
    | ScaledAllometric
    | Allometric
    | IPF
    | McCulloch
    | Gender
    | Lift


allColumns : List Column
allColumns =
    [ Gender
    , Lift
    , LiftedKilos
    , BodyKilos
    , LiftedPounds
    , BodyPounds
    , Wilks
    , Allometric
    , IPF
    , McCulloch
    , ScaledAllometric
    ]


initCurrentColumns : List Column
initCurrentColumns =
    [ Gender
    , Lift
    , LiftedKilos
    , BodyKilos
    , LiftedPounds
    , BodyPounds
    , Wilks
    , Allometric
    , IPF
    , McCulloch
    , ScaledAllometric
    ]


initTableColumns : List Column
initTableColumns =
    [ Gender
    , Lift
    , LiftedKilos
    , BodyKilos
    , Wilks
    , IPF
    , ScaledAllometric
    , Allometric
    ]


unitSeparatorSpace =
    String.fromChar '\u{200A}'


columnToColumnLabel : Column -> String
columnToColumnLabel column =
    case column of
        BodyKilos ->
            "BW" ++ unitSeparatorSpace ++ "(kg)"

        BodyPounds ->
            "BW" ++ unitSeparatorSpace ++ "(lb)"

        ScaledAllometric ->
            "Sc. Allo."

        Allometric ->
            "Allo."

        x ->
            columnToToggleLabel x


columnToToggleLabel : Column -> String
columnToToggleLabel column =
    case column of
        Gender ->
            "Gender"

        Lift ->
            "Lift"

        LiftedKilos ->
            "Lift (kg)"

        BodyKilos ->
            "Bodyweight (kg)"

        LiftedPounds ->
            "Lift (lb)"

        BodyPounds ->
            "Bodyweight (lb)"

        Wilks ->
            "Wilks"

        ScaledAllometric ->
            "Scaled Allometric"

        Allometric ->
            "Allometric"

        IPF ->
            "IPF"

        McCulloch ->
            "McCulloch"


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

        LiftedKilos ->
            .feat >> .liftedKilos >> floatToProgress maxes.feat.liftedKilos

        BodyKilos ->
            .feat >> .bodyKilos >> floatToProgress maxes.feat.bodyKilos

        LiftedPounds ->
            .feat >> .liftedPounds >> floatToProgress maxes.feat.liftedPounds

        BodyPounds ->
            .feat >> .bodyPounds >> floatToProgress maxes.feat.bodyPounds

        Wilks ->
            .wilks >> maybeFloatToProgress maxes.wilks

        ScaledAllometric ->
            .scaledAllometric >> maybeFloatToProgress maxes.scaledAllometric

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

            LiftedKilos ->
                .feat >> .liftedKilos >> floatToString

            BodyKilos ->
                .feat >> .bodyKilos >> floatToString

            LiftedPounds ->
                .feat >> .liftedPounds >> floatToString

            BodyPounds ->
                .feat >> .bodyPounds >> floatToString

            Wilks ->
                .wilks >> maybeFloatToString

            ScaledAllometric ->
                .scaledAllometric >> maybeFloatToString

            Allometric ->
                .allometric >> maybeFloatToString

            IPF ->
                .ipf >> maybeFloatToString

            McCulloch ->
                .mcCulloch >> maybeFloatToString
