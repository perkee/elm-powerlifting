module Feat exposing
    ( Feat
    , Gender(..)
    , Lift(..)
    , MassUnit(..)
    , genderToString
    , liftToString
    , massToKilos
    , massToPounds
    )


type MassUnit
    = KG
    | LBM


type Lift
    = Squat
    | Bench
    | Deadlift
    | Total


type alias Feat =
    { bodyKilos : Float
    , bodyPounds : Float
    , liftedKilos : Float
    , liftedPounds : Float
    , gender : Gender
    , lift : Lift
    , age : Maybe Float
    }


type Gender
    = Male
    | Female
    | GNC


genderToString : Gender -> String
genderToString gender =
    case gender of
        Male ->
            "Male"

        Female ->
            "Female"

        GNC ->
            "Other"


liftToString : Lift -> String
liftToString lift =
    case lift of
        Squat ->
            "Squat"

        Bench ->
            "Bench"

        Deadlift ->
            "Deadlift"

        Total ->
            "Total"


poundsPerKilo : Float
poundsPerKilo =
    0.45359237


massToKilos : MassUnit -> Float -> Float
massToKilos u m =
    case u of
        KG ->
            m

        LBM ->
            m * poundsPerKilo


massToPounds : MassUnit -> Float -> Float
massToPounds u m =
    case u of
        KG ->
            m / poundsPerKilo

        LBM ->
            m
