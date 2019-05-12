module Column exposing (Column(..), columnToRecordToText, columnToToggleLabel, initColumns)

import Feat exposing (genderToString, liftToString)
import Html as H exposing (Html)
import Library exposing (thrush)
import Renderer exposing (floatToString, maybeFloatToString, rowsToHeadedTable, textual)
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


initColumns : List Column
initColumns =
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
