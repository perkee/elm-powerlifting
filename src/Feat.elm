module Feat exposing
    ( Equipment(..)
    , ExtremeFeat(..)
    , Feat(..)
    , Gender(..)
    , LastFeatPromise(..)
    , Lift(..)
    , SingleExtremeFeatInternal(..)
    , SingleFeatInternal(..)
    , SumExtremeFeatInternal(..)
    , SumFeatInternal(..)
    , decode
    ,  decodeEquipment
       -- test

    ,  decodeGender
       -- test

    ,  decodeLift
       -- test

    , demoToComparable
    , equipmentToString
    , genderToString
    , liftToLetter
    , liftToString
    , max
    , serialize
    , toExtreme
    )

import Dict
import Json.Decode as D
import Json.Encode as E
import Library as L
import Mass exposing (Mass, MassUnit(..))


type Lift
    = Squat
    | Bench
    | Deadlift
    | Total


type alias Demographic =
    { bodyMass : Mass
    , gender : Gender
    , age : Maybe Float
    , note : String
    }


type alias LiftAttempt =
    { liftedMass : Mass
    , equipment : Equipment
    }


type alias SumLiftAttempt =
    { squat : LiftAttempt
    , bench : LiftAttempt
    , deadlift : LiftAttempt
    }


type Feat
    = Single SingleFeatInternal
    | Sum SumFeatInternal


type SingleFeatInternal
    = SingleFeatInternal Demographic Lift LiftAttempt


type SumFeatInternal
    = SumFeatInternal Demographic SumLiftAttempt


type DecoratedFeat
    = SingleDecoratedFeat Demographic Lift LiftAttempt SingleExtremeFeatInternal
    | SumDecoratedFeat Demographic SumLiftAttempt SingleExtremeFeatInternal


type ExtremeFeat
    = SingleExtremeFeat Demographic Lift LiftAttempt SingleExtremeFeatInternal
    | SumExtremeFeat Demographic SumLiftAttempt SumExtremeFeatInternal


type LastFeatPromise
    = SingleLast SingleExtremeFeatInternal
    | SumLast SumExtremeFeatInternal


type alias ComparableDemographic =
    { age : Maybe Float
    , bodyMass : Mass
    }


demoToComparable : Demographic -> ComparableDemographic
demoToComparable demo =
    { age = demo.age
    , bodyMass = demo.bodyMass
    }


type SumExtremeFeatInternal
    = SumExtremeFeatInternal
        ComparableDemographic
        { squat : Mass
        , bench : Mass
        , deadlift : Mass
        , total : Mass
        }


type SingleExtremeFeatInternal
    = SingleExtremeFeatInternal Lift ComparableDemographic Mass


toExtreme : Feat -> ExtremeFeat
toExtreme feat =
    case feat of
        Single (SingleFeatInternal demo lift attempt) ->
            SingleExtremeFeat demo lift attempt <|
                SingleExtremeFeatInternal
                    lift
                    (demoToComparable demo)
                    attempt.liftedMass

        Sum (SumFeatInternal demo { squat, bench, deadlift }) ->
            SumExtremeFeat demo { squat = squat, bench = bench, deadlift = deadlift } <|
                SumExtremeFeatInternal
                    (demoToComparable demo)
                    { squat = squat.liftedMass
                    , bench = bench.liftedMass
                    , deadlift = deadlift.liftedMass
                    , total =
                        Mass.sum
                            [ squat.liftedMass
                            , bench.liftedMass
                            , deadlift.liftedMass
                            ]
                    }


maxInternal : LastFeatPromise -> SumExtremeFeatInternal -> SumExtremeFeatInternal
maxInternal right (SumExtremeFeatInternal leftDemo leftMasses) =
    case right of
        SingleLast (SingleExtremeFeatInternal rightLift rightDemo rightLiftedMass) ->
            SumExtremeFeatInternal
                { age = L.maybeMax leftDemo.age rightDemo.age
                , bodyMass = Mass.max leftDemo.bodyMass rightDemo.bodyMass
                }
                (case rightLift of
                    Squat ->
                        { squat = Mass.max leftMasses.squat rightLiftedMass
                        , bench = leftMasses.bench
                        , deadlift = leftMasses.deadlift
                        , total = leftMasses.total
                        }

                    Bench ->
                        { bench = Mass.max leftMasses.bench rightLiftedMass
                        , squat = leftMasses.squat
                        , deadlift = leftMasses.deadlift
                        , total = leftMasses.total
                        }

                    Deadlift ->
                        { deadlift = Mass.max leftMasses.deadlift rightLiftedMass
                        , squat = leftMasses.squat
                        , bench = leftMasses.bench
                        , total = leftMasses.total
                        }

                    Total ->
                        { total = Mass.max leftMasses.total rightLiftedMass
                        , squat = leftMasses.squat
                        , bench = leftMasses.bench
                        , deadlift = leftMasses.deadlift
                        }
                )

        SumLast (SumExtremeFeatInternal rightDemo rightMasses) ->
            SumExtremeFeatInternal
                { age = L.maybeMax leftDemo.age rightDemo.age
                , bodyMass = Mass.max leftDemo.bodyMass rightDemo.bodyMass
                }
                { squat = Mass.max leftMasses.squat rightMasses.squat
                , bench = Mass.max leftMasses.bench rightMasses.bench
                , deadlift = Mass.max leftMasses.deadlift rightMasses.deadlift
                , total = Mass.max leftMasses.total rightMasses.total
                }


max : List LastFeatPromise -> SumExtremeFeatInternal
max =
    List.foldl
        maxInternal
        (SumExtremeFeatInternal
            { bodyMass = Mass.zero
            , age = Nothing
            }
            { squat = Mass.zero
            , bench = Mass.zero
            , deadlift = Mass.zero
            , total = Mass.zero
            }
        )


serialize : Feat -> E.Value
serialize feat =
    E.object <|
        case feat of
            Single (SingleFeatInternal demo lift liftAttempt) ->
                serializeDemographic demo
                    ++ [ ( "lift"
                         , E.string <|
                            case lift of
                                Squat ->
                                    "S"

                                Bench ->
                                    "B"

                                Deadlift ->
                                    "D"

                                Total ->
                                    "T"
                         )
                       , ( "type", E.string "single" )
                       ]
                    ++ serializeLiftAttempt liftAttempt

            Sum (SumFeatInternal demo { squat, bench, deadlift }) ->
                serializeDemographic demo
                    ++ [ ( "type", E.string "sum" )
                       , ( "squat", E.object <| serializeLiftAttempt squat )
                       , ( "bench", E.object <| serializeLiftAttempt bench )
                       , ( "deadlift", E.object <| serializeLiftAttempt deadlift )
                       ]


serializeDemographic : Demographic -> List ( String, E.Value )
serializeDemographic d =
    [ ( "bodyMass", Mass.serialize d.bodyMass )
    , ( "gender"
      , E.string <|
            case d.gender of
                Male ->
                    "M"

                Female ->
                    "F"

                GNC ->
                    "GNC"
      )
    , ( "age"
      , case d.age of
            Just age ->
                E.float age

            Nothing ->
                E.null
      )
    , ( "note", E.string d.note )
    ]


serializeLiftAttempt : LiftAttempt -> List ( String, E.Value )
serializeLiftAttempt la =
    [ ( "equipment"
      , E.string <|
            case la.equipment of
                Raw ->
                    "R"

                SinglePly ->
                    "SP"
      )
    , ( "liftedMass", Mass.serialize la.liftedMass )
    ]


decode : D.Decoder Feat
decode =
    D.maybe (D.field "type" D.string)
        |> D.andThen versionedDecode


versionedDecode : Maybe String -> D.Decoder Feat
versionedDecode maybeType =
    case Maybe.withDefault "single" maybeType of
        "single" ->
            D.map7 buildSingle
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

        "sum" ->
            D.map7 buildSum
                (D.field "bodyMass" Mass.decode)
                (D.field "gender" decodeGender)
                (D.maybe (D.field "age" D.float))
                (D.map
                    -- If the note is indecipherable OR absent
                    --that's cool just make it an empty string
                    (Maybe.withDefault "")
                    (D.maybe <| D.field "note" D.string)
                )
                (D.field "squat" decodeLiftAttempt)
                (D.field "bench" decodeLiftAttempt)
                (D.field "deadlift" decodeLiftAttempt)

        s ->
            D.fail <|
                "Trying to decode feat, but type \""
                    ++ s
                    ++ "\" is unknown"


buildSingle :
    Mass
    -> Mass
    -> Gender
    -> Lift
    -> Maybe Float
    -> Equipment
    -> String
    -> Feat
buildSingle bodyMass liftedMass gender lift age equipment note =
    Single <|
        SingleFeatInternal
            { bodyMass = bodyMass
            , gender = gender
            , age = age
            , note = note
            }
            lift
        <|
            LiftAttempt liftedMass equipment


buildSum :
    Mass
    -> Gender
    -> Maybe Float
    -> String
    -> LiftAttempt
    -> LiftAttempt
    -> LiftAttempt
    -> Feat
buildSum bodyMass gender age note squat bench deadlift =
    Sum <|
        SumFeatInternal
            { bodyMass = bodyMass
            , gender = gender
            , age = age
            , note = note
            }
            { squat = squat
            , bench = bench
            , deadlift = deadlift
            }


decodeLiftAttempt : D.Decoder LiftAttempt
decodeLiftAttempt =
    D.map2 LiftAttempt
        (D.field "liftedMass" Mass.decode)
        (D.field "equipment" decodeEquipment)


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
    | GNC -- non compliant! https://tempest-caller.tumblr.com/post/189269177767


type Equipment
    = Raw
    | SinglePly


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
