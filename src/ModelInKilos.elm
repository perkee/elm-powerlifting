module ModelInKilos exposing
    ( ActualModelInKilos
    , Lift(..)
    , MassUnit(..)
    , ModelInKilos
    , Gender(..)
    , massToKilos
    )


type MassUnit
    = KG
    | LBM


type Lift
    = Squat
    | Bench
    | Deadlift
    | Total


type alias ActualModelInKilos =
    { bodyMass : Float
    , liftedMass : Float
    , gender : Gender
    , lift : Lift
    }


type alias ModelInKilos =
    Maybe ActualModelInKilos


type Gender
    = Male
    | Female


massToKilos : MassUnit -> Float -> Float
massToKilos u m =
    case u of
        KG ->
            m

        LBM ->
            m / 2.204623
