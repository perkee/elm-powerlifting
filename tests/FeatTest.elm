module FeatTest exposing (suite)

import Expect exposing (Expectation)
import Feat exposing (..)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as D
import Json.Encode as E
import Mass exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Feat"
        [ describe "toString helpers"
            [ describe "equipmentToString"
                [ test "raw" <|
                    \_ ->
                        equipmentToString Raw
                            |> Expect.equal "Raw"
                , test "single ply" <|
                    \_ ->
                        equipmentToString SinglePly
                            |> Expect.equal "1-ply"
                ]
            , describe "liftToString"
                [ test "squat" <|
                    \_ ->
                        liftToString Squat
                            |> Expect.equal "Squat"
                , test "Bench" <|
                    \_ ->
                        liftToString Bench
                            |> Expect.equal "Bench"
                , test "Deadlift" <|
                    \_ ->
                        liftToString Deadlift
                            |> Expect.equal "Deadlift"
                , test "Total" <|
                    \_ ->
                        liftToString Total
                            |> Expect.equal "Total"
                ]
            , describe "liftToLetter"
                [ test "squat" <|
                    \_ ->
                        liftToLetter Squat
                            |> Expect.equal "S"
                , test "Bench" <|
                    \_ ->
                        liftToLetter Bench
                            |> Expect.equal "B"
                , test "Deadlift" <|
                    \_ ->
                        liftToLetter Deadlift
                            |> Expect.equal "D"
                , test "Total" <|
                    \_ ->
                        liftToLetter Total
                            |> Expect.equal "T"
                ]
            ]
        , describe "decoders"
            [ describe "stringToEquipmentDecoder"
                [ test "raw" <|
                    \_ ->
                        D.decodeString decodeEquipment "\"R\""
                            |> Expect.equal (Ok Raw)
                , test "Single Ply" <|
                    \_ ->
                        D.decodeString decodeEquipment "\"SP\""
                            |> Expect.equal (Ok SinglePly)
                , test "Fail" <|
                    \_ ->
                        D.decodeString decodeEquipment "\"something\""
                            |> Expect.equal
                                (Err
                                    (D.Failure "Unknown Equipment something"
                                        (E.string "something")
                                    )
                                )
                ]
            , describe "stringToGenderDecoder"
                [ test "Male" <|
                    \_ ->
                        D.decodeString decodeGender "\"M\""
                            |> Expect.equal (Ok Male)
                , test "Female" <|
                    \_ ->
                        D.decodeString decodeGender "\"F\""
                            |> Expect.equal (Ok Female)
                , test "GNC" <|
                    \_ ->
                        D.decodeString decodeGender "\"GNC\""
                            |> Expect.equal (Ok GNC)
                , test "fail" <|
                    \_ ->
                        D.decodeString decodeGender "\"something\""
                            |> Expect.equal
                                (Err
                                    (D.Failure
                                        "Unknown Gender something"
                                        (E.string "something")
                                    )
                                )
                ]
            , describe "decodeLift"
                [ test "Squat" <|
                    \_ ->
                        D.decodeString decodeLift "\"S\""
                            |> Expect.equal (Ok Squat)
                , test "Bench" <|
                    \_ ->
                        D.decodeString decodeLift "\"B\""
                            |> Expect.equal (Ok Bench)
                , test "DL" <|
                    \_ ->
                        D.decodeString decodeLift "\"D\""
                            |> Expect.equal (Ok Deadlift)
                , test "T" <|
                    \_ ->
                        D.decodeString decodeLift "\"T\""
                            |> Expect.equal (Ok Total)
                , test "fail" <|
                    \_ ->
                        D.decodeString decodeLift "\"???\""
                            |> Expect.equal
                                (Err
                                    (D.Failure
                                        "Unknown Lift ???"
                                        (E.string "???")
                                    )
                                )
                ]
            , describe "decode"
                [ test "implicit single encoding" <|
                    \_ ->
                        D.decodeString Feat.decode
                            """
                                {
                                    "bodyMass": {
                                        "number": 123.21,
                                        "unit": "KG"
                                    },
                                    "liftedMass": {
                                        "number": 321.23,
                                        "unit": "LBM"
                                    },
                                    "age": 22.22,
                                    "gender": "M",
                                    "lift": "S",
                                    "equipment": "R",
                                    "note": "the note"
                                }
                            """
                            |> Expect.equal
                                (Ok <|
                                    Single
                                        { bodyMass =
                                            Mass.fromUnitAndFloat
                                                KG
                                                123.21
                                        , gender = Male
                                        , age = Just 22.22
                                        , note = "the note"
                                        }
                                        Squat
                                        { equipment = Raw
                                        , liftedMass =
                                            Mass.fromUnitAndFloat
                                                LBM
                                                321.23
                                        }
                                )
                , test "explicit single encoding" <|
                    \_ ->
                        D.decodeString Feat.decode
                            """
                                {
                                    "bodyMass": {
                                        "number": 123.21,
                                        "unit": "KG"
                                    },
                                    "liftedMass": {
                                        "number": 321.23,
                                        "unit": "LBM"
                                    },
                                    "age": 22.22,
                                    "gender": "M",
                                    "lift": "S",
                                    "equipment": "R",
                                    "note": "the note",
                                    "type": "single"
                                }
                            """
                            |> Expect.equal
                                (Ok <|
                                    Single
                                        { bodyMass =
                                            Mass.fromUnitAndFloat
                                                KG
                                                123.21
                                        , gender = Male
                                        , age = Just 22.22
                                        , note = "the note"
                                        }
                                        Squat
                                        { equipment = Raw
                                        , liftedMass =
                                            Mass.fromUnitAndFloat
                                                LBM
                                                321.23
                                        }
                                )
                , test "explicit sum encoding" <|
                    \_ ->
                        D.decodeString Feat.decode
                            """
                                {
                                    "bodyMass": {
                                        "number": 123.21,
                                        "unit": "KG"
                                    },
                                    "liftedMass": {
                                        "number": 321.23,
                                        "unit": "LBM"
                                    },
                                    "age": 22.22,
                                    "gender": "M",
                                    "note": "the note",
                                    "type": "sum",
                                    "squat" : {
                                        "equipment": "R",
                                        "liftedMass": {
                                            "number": 111.11,
                                            "unit": "KG"
                                        }
                                    },
                                    "bench" : {
                                        "equipment": "R",
                                        "liftedMass": {
                                            "number": 222.22,
                                            "unit": "LBM"
                                        }
                                    },
                                    "deadlift" : {
                                        "equipment": "SP",
                                        "liftedMass": {
                                            "number": 333.33,
                                            "unit": "KG"
                                        }
                                    }
                                }
                            """
                            |> Expect.equal
                                (Ok <|
                                    Sum
                                        { bodyMass =
                                            Mass.fromUnitAndFloat
                                                KG
                                                123.21
                                        , gender = Male
                                        , age = Just 22.22
                                        , note = "the note"
                                        }
                                        { squat =
                                            { equipment = Raw
                                            , liftedMass =
                                                Mass.fromUnitAndFloat
                                                    KG
                                                    111.11
                                            }
                                        , bench =
                                            { equipment = Raw
                                            , liftedMass =
                                                Mass.fromUnitAndFloat
                                                    LBM
                                                    222.22
                                            }
                                        , deadlift =
                                            { equipment = SinglePly
                                            , liftedMass =
                                                Mass.fromUnitAndFloat
                                                    KG
                                                    333.33
                                            }
                                        }
                                )
                , test "symmetric sum start with encode" <|
                    \_ ->
                        let
                            feat =
                                Sum
                                    { bodyMass =
                                        Mass.fromUnitAndFloat
                                            KG
                                            123.21
                                    , gender = Male
                                    , age = Just 22.22
                                    , note = "the note"
                                    }
                                    { squat =
                                        { equipment = Raw
                                        , liftedMass =
                                            Mass.fromUnitAndFloat
                                                KG
                                                111.11
                                        }
                                    , bench =
                                        { equipment = Raw
                                        , liftedMass =
                                            Mass.fromUnitAndFloat
                                                LBM
                                                222.22
                                        }
                                    , deadlift =
                                        { equipment = SinglePly
                                        , liftedMass =
                                            Mass.fromUnitAndFloat
                                                KG
                                                333.33
                                        }
                                    }
                        in
                        feat
                            |> Feat.serialize
                            |> D.decodeValue Feat.decode
                            |> Expect.equal
                                (Ok <| feat)
                , test "symmetric single start with encode" <|
                    \_ ->
                        let
                            feat =
                                Single
                                    { bodyMass =
                                        Mass.fromUnitAndFloat
                                            KG
                                            123.21
                                    , gender = Male
                                    , age = Just 22.22
                                    , note = "the note"
                                    }
                                    Squat
                                    { equipment = Raw
                                    , liftedMass =
                                        Mass.fromUnitAndFloat
                                            LBM
                                            321.23
                                    }
                        in
                        feat
                            |> Feat.serialize
                            |> D.decodeValue Feat.decode
                            |> Expect.equal
                                (Ok <| feat)
                ]
            ]
        ]
