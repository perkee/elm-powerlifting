module Feat exposing
    ( Equipment(..)
    , Feat
    , Gender(..)
    , Lift(..)
    , MassUnit(..)
    , equipmentToString
    , genderToString
    , liftToLetter
    , liftToString
    , massToKilos
    , massToPounds
    , testFeats
    , toggleMassUnit
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
      , bodyPounds = massToPounds KG 100
      , liftedKilos = 400
      , liftedPounds = massToPounds KG 400
      , gender = Male
      , lift = Bench
      , age = Just 40
      , equipment = SinglePly
      , note = "first"
      }
    , { bodyKilos = 99
      , bodyPounds = massToPounds KG 99
      , liftedKilos = 1007.5
      , liftedPounds = massToPounds KG 1007.5
      , gender = Female
      , lift = Total
      , age = Just 45
      , equipment = Raw
      , note = "second"
      }
    , { bodyKilos = 60
      , bodyPounds = massToPounds KG 60
      , liftedKilos = 505
      , liftedPounds = massToPounds KG 505
      , gender = GNC
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


liftToLetter : Lift -> String
liftToLetter =
    liftToString >> String.left 1


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


toggleMassUnit : MassUnit -> MassUnit
toggleMassUnit unit =
    case unit of
        KG ->
            LBM

        LBM ->
            KG
