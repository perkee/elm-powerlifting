module Scores exposing
    ( ExtremeRecord
    , Record
    , featToRecord
    , max
    )

import Array
import Feat
    exposing
        ( Equipment(..)
        , Feat
        , Gender(..)
        , Lift(..)
        )
import Library as L
import Mass


type alias ByMasses a =
    { a
        | bodyMass : Mass.Mass
        , liftedMass : Mass.Mass
    }


type alias ByGender a =
    { a | gender : Gender }


type alias ByEquipment a =
    { a | equipment : Equipment }


type alias ByLift a =
    { a | lift : Lift }


type alias ByAge a =
    { a | age : Maybe Float }


type alias AllometricInput a =
    ByMasses a


type alias WilksInput a =
    ByGender (AllometricInput a)


type alias McInput a =
    ByAge (WilksInput a)


type alias IpfInput a =
    ByEquipment (ByLift (WilksInput a))


type alias EnoughInput a =
    ByAge (IpfInput a)


type Input a
    = SingleInput (EnoughInput a) Feat.SingleExtremeFeatInternal
    | SumInput
        { squat : EnoughInput a
        , bench : EnoughInput a
        , deadlift : EnoughInput a
        , total : EnoughInput a
        }
        Feat.SumExtremeFeatInternal



-- Scores


type Score
    = Wilks Float
    | ScaledAllometricIpf Float
    | ScaledAllometricAtr Float
    | Ipf Float
    | McCulloch Float
    | Allometric Float
    | NoScore


type alias SingleRecordScores =
    { wilks : Maybe Float
    , scaledAllometricIpf : Maybe Float
    , scaledAllometricAtr : Maybe Float
    , allometric : Maybe Float
    , ipf : Maybe Float
    , mcCulloch : Maybe Float
    }


maxSingleRecordScores : SingleRecordScores -> SingleRecordScores -> SingleRecordScores
maxSingleRecordScores a b =
    { wilks = L.maybeMax a.wilks b.wilks
    , scaledAllometricIpf = L.maybeMax a.scaledAllometricIpf b.scaledAllometricIpf
    , scaledAllometricAtr = L.maybeMax a.scaledAllometricAtr b.scaledAllometricAtr
    , allometric = L.maybeMax a.allometric b.allometric
    , ipf = L.maybeMax a.ipf b.ipf
    , mcCulloch = L.maybeMax a.mcCulloch b.mcCulloch
    }


emptySingleRecordScores : SingleRecordScores
emptySingleRecordScores =
    { wilks = Nothing
    , scaledAllometricIpf = Nothing
    , scaledAllometricAtr = Nothing
    , allometric = Nothing
    , ipf = Nothing
    , mcCulloch = Nothing
    }


type alias SumRecordScores =
    { squat : SingleRecordScores
    , bench : SingleRecordScores
    , deadlift : SingleRecordScores
    , total : SingleRecordScores
    }


emptySumRecordScores : SumRecordScores
emptySumRecordScores =
    { squat = emptySingleRecordScores
    , bench = emptySingleRecordScores
    , deadlift = emptySingleRecordScores
    , total = emptySingleRecordScores
    }


type Record
    = Single Feat.SingleExtremeFeatInternal SingleRecordScores
    | Sum Feat.SumExtremeFeatInternal SumRecordScores


type ExtremeRecord
    = ExtremeRecord Feat.SumExtremeFeatInternal SumRecordScores


max : List Record -> ExtremeRecord
max records =
    ExtremeRecord
        (List.map recordToLastFeat records |> Feat.max)
        (List.foldl maxScores emptySumRecordScores records)


maxScores : Record -> SumRecordScores -> SumRecordScores
maxScores new currentScores =
    case new of
        Single (Feat.SingleExtremeFeatInternal lift _ _) newSingleScores ->
            case lift of
                Squat ->
                    { squat = maxSingleRecordScores currentScores.squat newSingleScores
                    , bench = currentScores.bench
                    , deadlift = currentScores.deadlift
                    , total = currentScores.total
                    }

                Bench ->
                    { bench = maxSingleRecordScores currentScores.bench newSingleScores
                    , squat = currentScores.squat
                    , deadlift = currentScores.deadlift
                    , total = currentScores.total
                    }

                Deadlift ->
                    { deadlift = maxSingleRecordScores currentScores.deadlift newSingleScores
                    , squat = currentScores.squat
                    , bench = currentScores.bench
                    , total = currentScores.total
                    }

                Total ->
                    { total = maxSingleRecordScores currentScores.total newSingleScores
                    , squat = currentScores.squat
                    , bench = currentScores.bench
                    , deadlift = currentScores.deadlift
                    }

        Sum _ newSumScores ->
            { squat = maxSingleRecordScores currentScores.squat newSumScores.squat
            , bench = maxSingleRecordScores currentScores.bench newSumScores.bench
            , deadlift = maxSingleRecordScores currentScores.deadlift newSumScores.deadlift
            , total = maxSingleRecordScores currentScores.total newSumScores.total
            }


recordToLastFeat : Record -> Feat.LastFeatPromise
recordToLastFeat r =
    case r of
        Single internal _ ->
            -- internal : Feat.SingleExtremeFeatInternal
            Feat.SingleLast internal

        Sum internal _ ->
            -- internal : Feat.SumExtremeFeatInternal
            Feat.SumLast internal


featToInput : Feat -> Input {}
featToInput feat =
    case Feat.toExtreme feat of
        Feat.SingleExtremeFeat demo lift result internal ->
            SingleInput
                { age = demo.age
                , bodyMass = demo.bodyMass
                , gender = demo.gender
                , liftedMass = result.liftedMass
                , equipment = result.equipment
                , lift = lift
                }
                internal

        Feat.SumExtremeFeat demo { squat, bench, deadlift } internal ->
            SumInput
                { squat =
                    { bodyMass = demo.bodyMass
                    , age = demo.age
                    , gender = demo.gender
                    , liftedMass = squat.liftedMass
                    , equipment = squat.equipment
                    , lift = Squat
                    }
                , bench =
                    { bodyMass = demo.bodyMass
                    , age = demo.age
                    , gender = demo.gender
                    , liftedMass = bench.liftedMass
                    , equipment = bench.equipment
                    , lift = Bench
                    }
                , deadlift =
                    { bodyMass = demo.bodyMass
                    , age = demo.age
                    , gender = demo.gender
                    , liftedMass = deadlift.liftedMass
                    , equipment = deadlift.equipment
                    , lift = Deadlift
                    }
                , total =
                    { bodyMass = demo.bodyMass
                    , age = demo.age
                    , gender = demo.gender
                    , liftedMass =
                        Mass.sum
                            [ squat.liftedMass
                            , bench.liftedMass
                            , deadlift.liftedMass
                            ]
                    , equipment = deadlift.equipment
                    , lift = Deadlift
                    }
                }
                internal


featToRecord : Feat -> Record
featToRecord feat =
    case featToInput feat of
        SingleInput input featInternal ->
            Single featInternal
                { wilks = wilks input
                , scaledAllometricIpf =
                    scaledAllometricIpf input
                , scaledAllometricAtr =
                    scaledAllometricAtr input
                , allometric =
                    allometric input
                , ipf =
                    ipf input
                , mcCulloch =
                    mcCulloch input
                }

        SumInput { squat, bench, deadlift, total } featInternal ->
            Sum featInternal
                { squat =
                    { wilks = wilks squat
                    , scaledAllometricIpf =
                        scaledAllometricIpf squat
                    , scaledAllometricAtr =
                        scaledAllometricAtr squat
                    , allometric =
                        allometric squat
                    , ipf =
                        ipf squat
                    , mcCulloch =
                        mcCulloch squat
                    }
                , bench =
                    { wilks = wilks bench
                    , scaledAllometricIpf =
                        scaledAllometricIpf bench
                    , scaledAllometricAtr =
                        scaledAllometricAtr bench
                    , allometric =
                        allometric bench
                    , ipf =
                        ipf bench
                    , mcCulloch =
                        mcCulloch bench
                    }
                , deadlift =
                    { wilks = wilks deadlift
                    , scaledAllometricIpf =
                        scaledAllometricIpf deadlift
                    , scaledAllometricAtr =
                        scaledAllometricAtr deadlift
                    , allometric =
                        allometric deadlift
                    , ipf =
                        ipf deadlift
                    , mcCulloch =
                        mcCulloch deadlift
                    }
                , total =
                    { wilks = wilks total
                    , scaledAllometricIpf =
                        scaledAllometricIpf total
                    , scaledAllometricAtr =
                        scaledAllometricAtr total
                    , allometric =
                        allometric total
                    , ipf =
                        ipf total
                    , mcCulloch =
                        mcCulloch total
                    }
                }



-- Allometric Scaling Score
-- Unscaled Allometric


allometric : AllometricInput a -> Maybe Float
allometric m =
    Mass.toKilos m.bodyMass
        ^ (-2 / 3)
        * Mass.toKilos m.liftedMass
        |> Just



-- Scaled Allometric


type NuckolsFed
    = IPF
    | AllTimeRaw


allometricCoefficient : NuckolsFed -> IpfInput a -> Maybe Float
allometricCoefficient fed feat =
    case fed of
        AllTimeRaw ->
            case ( feat.lift, feat.gender, feat.equipment ) of
                ( _, GNC, _ ) ->
                    Nothing

                ( _, _, SinglePly ) ->
                    Nothing

                ( Squat, Male, Raw ) ->
                    Just 3.870519111

                ( Squat, Female, Raw ) ->
                    Just 5.672829804

                ( Bench, Male, Raw ) ->
                    Just 5.435740026

                ( Bench, Female, Raw ) ->
                    Just 7.493817001

                ( Deadlift, Male, Raw ) ->
                    Just 3.901043709

                ( Deadlift, Female, Raw ) ->
                    Just 5.28914498

                ( Total, Male, Raw ) ->
                    Just 1.675105545

                ( Total, Female, Raw ) ->
                    Just 2.306479733

        IPF ->
            case ( feat.lift, feat.gender, feat.equipment ) of
                ( _, GNC, _ ) ->
                    Nothing

                ( Squat, Male, Raw ) ->
                    Just 6.487682129

                ( Squat, Female, Raw ) ->
                    Just 8.540082411

                ( Bench, Male, Raw ) ->
                    Just 8.373410442

                ( Bench, Female, Raw ) ->
                    Just 11.26896531

                ( Deadlift, Male, Raw ) ->
                    Just 5.510559406

                ( Deadlift, Female, Raw ) ->
                    Just 7.164206454

                ( Total, Male, Raw ) ->
                    Just 2.292801981

                ( Total, Female, Raw ) ->
                    Just 3.195981761

                ( Squat, Male, SinglePly ) ->
                    Just 4.796198362

                ( Bench, Male, SinglePly ) ->
                    Just 5.875342993

                ( Deadlift, Male, SinglePly ) ->
                    Just 5.217770257

                ( Total, Male, SinglePly ) ->
                    Just 1.947627512

                ( Squat, Female, SinglePly ) ->
                    Just 8.540082411

                ( Bench, Female, SinglePly ) ->
                    Just 11.26896531

                ( Deadlift, Female, SinglePly ) ->
                    Just 7.164206454

                ( Total, Female, SinglePly ) ->
                    Just 3.195981761


scaledAllometricIpf : IpfInput a -> Maybe Float
scaledAllometricIpf feat =
    case ( allometricCoefficient IPF feat, allometric feat ) of
        ( Just coefficient, Just unscaled ) ->
            unscaled
                * coefficient
                |> Just

        ( _, _ ) ->
            Nothing


scaledAllometricAtr : IpfInput a -> Maybe Float
scaledAllometricAtr feat =
    case ( allometricCoefficient AllTimeRaw feat, allometric feat ) of
        ( Just coefficient, Just unscaled ) ->
            unscaled
                * coefficient
                |> Just

        ( _, _ ) ->
            Nothing



-- IPF


ipf : IpfInput a -> Maybe Float
ipf m =
    if abs (Mass.toKilos m.liftedMass) < 0.25 then
        Just 0

    else
        let
            scale =
                Mass.toKilos m.bodyMass |> logBase e |> (*)
        in
        case ipfCoefficients m of
            Just cs ->
                500
                    + 100
                    * (Mass.toKilos m.liftedMass - (scale cs.c1 - cs.c2))
                    / (scale cs.c3 - cs.c4)
                    |> Just

            Nothing ->
                Nothing


ipfCoefficients :
    IpfInput a
    ->
        Maybe
            { c1 : Float
            , c2 : Float
            , c3 : Float
            , c4 : Float
            }
ipfCoefficients m =
    case ( m.lift, m.gender, m.equipment ) of
        ( _, GNC, _ ) ->
            Nothing

        ( Squat, Male, Raw ) ->
            Just { c1 = 123.1, c2 = 363.085, c3 = 25.1667, c4 = 75.4311 }

        ( Squat, Female, Raw ) ->
            Just { c1 = 50.479, c2 = 105.632, c3 = 19.1846, c4 = 56.2215 }

        ( Bench, Male, Raw ) ->
            Just { c1 = 86.4745, c2 = 259.155, c3 = 17.5785, c4 = 53.122 }

        ( Bench, Female, Raw ) ->
            Just { c1 = 25.0485, c2 = 43.848, c3 = 6.7172, c4 = 13.952 }

        ( Deadlift, Male, Raw ) ->
            Just { c1 = 103.5355, c2 = 244.765, c3 = 15.3714, c4 = 31.5022 }

        ( Deadlift, Female, Raw ) ->
            Just { c1 = 47.136, c2 = 67.349, c3 = 9.1555, c4 = 13.67 }

        ( Total, Male, Raw ) ->
            Just { c1 = 310.67, c2 = 857.785, c3 = 53.216, c4 = 147.0835 }

        ( Total, Female, Raw ) ->
            Just { c1 = 125.1435, c2 = 228.03, c3 = 34.5246, c4 = 86.8301 }

        ( Squat, Male, SinglePly ) ->
            Just { c1 = 150.485, c2 = 446.445, c3 = 36.5155, c4 = 103.7061 }

        ( Squat, Female, SinglePly ) ->
            Just { c1 = 74.6855, c2 = 171.585, c3 = 21.9475, c4 = 52.2948 }

        ( Bench, Male, SinglePly ) ->
            Just { c1 = 133.94, c2 = 441.465, c3 = 35.3938, c4 = 113.0057 }

        ( Bench, Female, SinglePly ) ->
            Just { c1 = 49.106, c2 = 124.209, c3 = 23.199, c4 = 67.4926 }

        ( Deadlift, Male, SinglePly ) ->
            Just { c1 = 110.135, c2 = 263.66, c3 = 14.996, c4 = 23.011 }

        ( Deadlift, Female, SinglePly ) ->
            Just { c1 = 51.002, c2 = 69.8265, c3 = 8.5802, c4 = 5.7258 }

        ( Total, Male, SinglePly ) ->
            Just { c1 = 387.265, c2 = 1121.28, c3 = 80.6324, c4 = 222.4896 }

        ( Total, Female, SinglePly ) ->
            Just { c1 = 176.58, c2 = 373.315, c3 = 48.4534, c4 = 110.0103 }



-- Wilks


wilksCoefficients : ByGender a -> Maybe (List Float)
wilksCoefficients f =
    case f.gender of
        Male ->
            Just
                [ -216.0475144
                , 16.2606339
                , -0.002388645
                , -0.00113732
                , 7.01863e-6
                , -1.291e-8
                ]

        Female ->
            Just
                [ 594.31747775582
                , -27.23842536447
                , 0.82112226871
                , -0.00930733913
                , 4.731582e-5
                , -9.054e-8
                ]

        GNC ->
            Nothing


polynomialMultiply : ByMasses a -> Int -> Float -> Float
polynomialMultiply f index const =
    const * Mass.toKilos f.bodyMass ^ toFloat index


wilks : WilksInput a -> Maybe Float
wilks f =
    case wilksCoefficients f of
        Just coefficients ->
            coefficients
                |> List.indexedMap (polynomialMultiply f)
                |> List.foldl (+) 0
                |> (/) (Mass.toKilos f.liftedMass * 500)
                |> Just

        Nothing ->
            Nothing



-- McCulloch


mcCulloch : McInput a -> Maybe Float
mcCulloch f =
    case ( Maybe.map mcCullochFactor f.age, wilks f ) of
        ( Just scale, Just score ) ->
            score * scale |> Just

        ( _, _ ) ->
            Nothing


mcCullochFactor : Float -> Float
mcCullochFactor age =
    let
        intAge =
            round age - 40
    in
    if intAge <= 0 then
        1

    else
        Array.get intAge
            >> Maybe.withDefault 2.549
        <|
            Array.fromList
                [ 1
                , 1.01
                , 1.02
                , 1.031
                , 1.043
                , 1.055
                , 1.068
                , 1.082
                , 1.097
                , 1.113
                , 1.13
                , 1.147
                , 1.165
                , 1.184
                , 1.204
                , 1.225
                , 1.246
                , 1.268
                , 1.291
                , 1.113
                , 1.315
                , 1.34
                , 1.366
                , 1.393
                , 1.421
                , 1.45
                , 1.48
                , 1.511
                , 1.543
                , 1.576
                , 1.61
                , 1.645
                , 1.681
                , 1.718
                , 1.756
                , 1.795
                , 1.835
                , 1.876
                , 1.918
                , 1.961
                , 2.005
                , 2.05
                , 2.096
                , 2.143
                , 2.19
                , 2.238
                , 2.287
                , 2.337
                , 2.388
                , 2.44
                , 2.494
                , 2.549
                ]
