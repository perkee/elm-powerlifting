module FeatTest exposing (suite)

import Expect exposing (Expectation)
import Feat exposing (..)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as D
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
                        stringToEquipmentDecoder "R"
                            |> Expect.equal (D.succeed Raw)
                ]
            ]
        ]
