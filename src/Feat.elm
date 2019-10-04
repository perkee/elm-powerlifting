module Feat exposing
    ( Equipment(..)
    , Feat
    , Gender(..)
    , Lift(..)
    , equipmentToString
    , genderToString
    , liftToLetter
    , liftToString
    , testFeats
    )

import Mass exposing (Mass, MassUnit(..))


type Lift
    = Squat
    | Bench
    | Deadlift
    | Total


type alias Feat =
    { bodyMass : Mass
    , liftedMass : Mass
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
    [ { bodyMass = Mass.fromUnitAndFloat KG 100
      , liftedMass = Mass.fromUnitAndFloat KG 400
      , gender = Male
      , lift = Bench
      , age = Just 40
      , equipment = SinglePly
      , note = "first"
      }
    , { bodyMass = Mass.fromUnitAndFloat KG 99
      , liftedMass = Mass.fromUnitAndFloat KG 1007.5
      , gender = Female
      , lift = Total
      , age = Just 45
      , equipment = Raw
      , note = "second"
      }
    , { bodyMass = Mass.fromUnitAndFloat KG 60
      , liftedMass = Mass.fromUnitAndFloat KG 505
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
