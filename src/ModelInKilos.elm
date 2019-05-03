module ModelInKilos exposing
    ( Feat
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


type alias Feat =
    { bodyMass : Float
    , liftedMass : Float
    , gender : Gender
    , lift : Lift
    }


type alias ModelInKilos =
    Maybe Feat


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
