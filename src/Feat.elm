module Feat exposing
    ( Feat
    , Gender(..)
    , Lift(..)
    , MassUnit(..)
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
    }


type Gender
    = Male
    | Female


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
