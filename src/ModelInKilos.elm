module ModelInKilos exposing
    ( ActualModelInKilos
    , Lift(..)
    , MassUnit(..)
    , ModelInKilos
    , Sex(..)
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
    , sex : Sex
    , lift : Lift
    }


type alias ModelInKilos =
    Maybe ActualModelInKilos


type Sex
    = Male
    | Female


massToKilos : MassUnit -> Float -> Float
massToKilos u m =
    case u of
        KG ->
            m

        LBM ->
            m / 2.204623
