module Feat exposing
    ( Equipment(..)
    , Feat
    , Gender(..)
    , Lift(..)
    , MassUnit(..)
    , equipmentToString
    , genderToString
    , liftToString
    , massToKilos
    , massToPounds
    , testFeats
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
    , equipment : Equipment
    , note : String
    }


type Gender
    = Male
    | Female
    | GNC


type Equipment
    = Raw
    | SinglePly


testFeats : List Feat
testFeats =
    [ { bodyKilos = 100
      , bodyPounds = 220
      , liftedKilos = 400
      , liftedPounds = 881
      , gender = Male
      , lift = Bench
      , age = Just 40
      , equipment = SinglePly
      , note = "first"
      }
    , { bodyKilos = 70
      , bodyPounds = 154
      , liftedKilos = 500
      , liftedPounds = 1101
      , gender = Male
      , lift = Total
      , age = Just 45
      , equipment = Raw
      , note = "second"
      }
    , { bodyKilos = 60
      , bodyPounds = 132
      , liftedKilos = 505
      , liftedPounds = 1112
      , gender = Male
      , lift = Total
      , age = Just 50
      , equipment = Raw
      , note = "third"
      }
    ]


genderToString : Gender -> String
genderToString gender =
    case gender of
        Male ->
            "Male"

        Female ->
            "Female"

        GNC ->
            "Other"


equipmentToString : Equipment -> String
equipmentToString equipment =
    case equipment of
        Raw ->
            "Raw"

        SinglePly ->
            "1-ply"


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
