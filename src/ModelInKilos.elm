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



-- type ModelInKilos
--     = Complete
--         { bodyMass : Float
--         , liftedMass : Float
--         , sex : Sex
--         }
--     | Incomplete


type alias ActualModelInKilos =
    { bodyMass : Float
    , liftedMass : Float
    , sex : Sex
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
