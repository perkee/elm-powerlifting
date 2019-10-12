module Feat exposing
    ( Equipment(..)
    , Feat
    , Gender(..)
    , Lift(..)
    , decode
    , equipmentToString
    , genderToString
    , liftToLetter
    , liftToString
    , serialize
    , testFeats
    )

import Dict
import Json.Decode as D
import Json.Encode as E
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


serialize : Feat -> E.Value
serialize f =
    E.object
        [ ( "bodyMass", Mass.serialize f.bodyMass )
        , ( "liftedMass", Mass.serialize f.liftedMass )
        , ( "gender"
          , E.string <|
                case f.gender of
                    Male ->
                        "M"

                    Female ->
                        "F"

                    GNC ->
                        "GNC"
          )
        , ( "lift"
          , E.string <|
                case f.lift of
                    Squat ->
                        "S"

                    Bench ->
                        "B"

                    Deadlift ->
                        "D"

                    Total ->
                        "T"
          )
        , ( "age"
          , case f.age of
                Just age ->
                    E.float age

                Nothing ->
                    E.null
          )
        , ( "equipment"
          , E.string <|
                case f.equipment of
                    Raw ->
                        "R"

                    SinglePly ->
                        "SP"
          )
        , ( "note", E.string f.note )
        ]


decode : D.Decoder Feat
decode =
    D.map7 Feat
        (D.field "bodyMass" Mass.decode)
        (D.field "liftedMass" Mass.decode)
        (D.field "gender" decodeGender)
        (D.field "lift" decodeLift)
        (D.maybe (D.field "age" D.float))
        (D.field "equipment" decodeEquipment)
        (D.map
            -- If the note is indecipherable OR absent
            --that's cool just make it an empty string
            (Maybe.withDefault "")
            (D.maybe <| D.field "note" D.string)
        )



-- (D.field "note" decodeNote)


decodeEquipment : D.Decoder Equipment
decodeEquipment =
    D.string |> D.andThen stringToEquipmentDecoder


decodeGender : D.Decoder Gender
decodeGender =
    D.string |> D.andThen stringToGenderDecoder


decodeLift : D.Decoder Lift
decodeLift =
    D.string |> D.andThen stringToLiftDecoder


stringToEquipmentDecoder : String -> D.Decoder Equipment
stringToEquipmentDecoder s =
    case
        Dict.fromList
            [ ( "R", Raw )
            , ( "SP", SinglePly )
            ]
            |> Dict.get s
    of
        Just e ->
            D.succeed e

        Nothing ->
            D.fail <| "Unknown Equipment " ++ s


stringToGenderDecoder : String -> D.Decoder Gender
stringToGenderDecoder s =
    case
        Dict.fromList
            [ ( "M", Male )
            , ( "F", Female )
            , ( "GNC", GNC )
            ]
            |> Dict.get s
    of
        Just g ->
            D.succeed g

        Nothing ->
            D.fail <| "Unknown Gender " ++ s


stringToLiftDecoder : String -> D.Decoder Lift
stringToLiftDecoder s =
    case
        Dict.fromList
            [ ( "S", Squat )
            , ( "B", Bench )
            , ( "D", Deadlift )
            , ( "T", Total )
            ]
            |> Dict.get s
    of
        Just l ->
            D.succeed l

        Nothing ->
            D.fail <| "Unknown Lift " ++ s


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
