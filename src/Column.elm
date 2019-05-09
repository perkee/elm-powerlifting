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
    , ScaledAllometric
    , Allometric
    , IPF
    , McCulloch
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
    case column of
        Gender ->
            .feat >> .gender >> genderToString >> H.text

        Lift ->
            .feat >> .lift >> liftToString >> H.text

        LiftedKilos ->
            .feat >> .liftedKilos >> floatToString >> H.text

        BodyKilos ->
            .feat >> .bodyKilos >> floatToString >> H.text

        LiftedPounds ->
            .feat >> .liftedPounds >> floatToString >> H.text

        BodyPounds ->
            .feat >> .bodyPounds >> floatToString >> H.text

        Wilks ->
            .wilks >> maybeFloatToString >> H.text

        ScaledAllometric ->
            .scaledAllometric >> maybeFloatToString >> H.text

        Allometric ->
            .allometric >> maybeFloatToString >> H.text

        IPF ->
            .ipf >> maybeFloatToString >> H.text

        McCulloch ->
            .mcCulloch >> maybeFloatToString >> H.text
