module Scores exposing
    ( Record
    , featToRecord
    , featToScores
    , maxRecord
    )

import Array
import Bootstrap.Table as Table
import Feat
    exposing
        ( Feat
        , Gender(..)
        , Lift(..)
        , MassUnit
        )
import Html as H exposing (Html)
import Library exposing (thrush)
import Renderer exposing (floatToString, maybeFloatToString, rowsToHeadedTable)



-- Scores


type Score
    = Wilks Float
    | ScaledAllometric Float
    | Ipf Float
    | McCulloch Float
    | Allometric Float
    | NoScore


type alias Scores =
    { wilks : Maybe Float
    , scaledAllometric : Maybe Float
    , allometric : Maybe Float
    , ipf : Maybe Float
    , mcCulloch : Maybe Float
    }


type alias Record =
    { feat : Feat
    , wilks : Maybe Float
    , scaledAllometric : Maybe Float
    , allometric : Maybe Float
    , ipf : Maybe Float
    , mcCulloch : Maybe Float
    }


featToRecord : Feat -> Record
featToRecord feat =
    feat
        |> featToScores
        |> List.foldr
            scoreToRecord
            { feat = feat
            , wilks = Nothing
            , scaledAllometric = Nothing
            , allometric = Nothing
            , ipf = Nothing
            , mcCulloch = Nothing
            }


scoreToRecord : Score -> Record -> Record
scoreToRecord score record =
    case score of
        Wilks s ->
            { record | wilks = Just s }

        ScaledAllometric s ->
            { record | scaledAllometric = Just s }

        Allometric s ->
            { record | allometric = Just s }

        Ipf s ->
            { record | ipf = Just s }

        McCulloch s ->
            { record | mcCulloch = Just s }

        NoScore ->
            record


featToScores : Feat -> List Score
featToScores feat =
    [ wilks
    , scaledAllometric
    , allometric
    , ipf
    , mcCulloch
    ]
        |> List.map (thrush feat)


maybeMax : Maybe Float -> Maybe Float -> Maybe Float
maybeMax current new =
    case ( current, new ) of
        ( Just c, Just n ) ->
            Just (max c n)

        ( Nothing, Just n ) ->
            Just n

        ( x, Nothing ) ->
            x


maxFeat : Feat -> Feat -> Feat
maxFeat current new =
    { bodyKilos = max current.bodyKilos new.bodyKilos
    , bodyPounds = max current.bodyPounds new.bodyPounds
    , liftedKilos = max current.liftedKilos new.liftedKilos
    , liftedPounds = max current.liftedPounds new.liftedPounds
    , gender = GNC
    , lift = Total
    , age = maybeMax current.age new.age
    }


maxRecord : List Record -> Record
maxRecord =
    List.foldl
        (\maxes record ->
            { maxes
                | wilks = maybeMax maxes.wilks record.wilks
                , scaledAllometric = maybeMax maxes.scaledAllometric record.scaledAllometric
                , allometric = maybeMax maxes.allometric record.allometric
                , ipf = maybeMax maxes.ipf record.ipf
                , mcCulloch = maybeMax maxes.mcCulloch record.mcCulloch
                , feat = maxFeat maxes.feat record.feat
            }
        )
        { feat =
            { bodyKilos = -1 / 0
            , bodyPounds = -1 / 0
            , liftedKilos = -1 / 0
            , liftedPounds = -1 / 0
            , gender = GNC
            , lift = Total
            , age = Nothing
            }
        , wilks = Nothing
        , scaledAllometric = Nothing
        , allometric = Nothing
        , ipf = Nothing
        , mcCulloch = Nothing
        }



-- Allometric Scaling Score
-- Unscaled Allometric


allometric : Feat -> Score
allometric m =
    m.bodyKilos
        ^ (-2 / 3)
        * m.liftedKilos
        |> Allometric



-- Scaled Allometric


allometricCoefficient : Feat -> Maybe Float
allometricCoefficient m =
    case ( m.lift, m.gender ) of
        ( _, GNC ) ->
            Nothing

        ( Squat, Male ) ->
            Just 6.487682129

        ( Squat, Female ) ->
            Just 8.540082411

        ( Bench, Male ) ->
            Just 8.373410442

        ( Bench, Female ) ->
            Just 11.26896531

        ( Deadlift, Male ) ->
            Just 5.510559406

        ( Deadlift, Female ) ->
            Just 7.164206454

        ( Total, Male ) ->
            Just 2.292801981

        ( Total, Female ) ->
            Just 3.195981761


scaledAllometric : Feat -> Score
scaledAllometric m =
    case ( allometricCoefficient m, allometric m ) of
        ( Just coefficient, Allometric unscaled ) ->
            unscaled
                * coefficient
                |> ScaledAllometric

        ( _, _ ) ->
            NoScore



-- IPF


ipf : Feat -> Score
ipf m =
    if abs m.liftedKilos < 0.25 then
        Ipf 0

    else
        let
            scale =
                m.bodyKilos |> logBase e |> (*)
        in
        case ipfCoefficients m of
            Just cs ->
                500
                    + 100
                    * (m.liftedKilos - (scale cs.c1 - cs.c2))
                    / (scale cs.c3 - cs.c4)
                    |> Ipf

            Nothing ->
                NoScore


ipfCoefficients :
    Feat
    ->
        Maybe
            { c1 : Float
            , c2 : Float
            , c3 : Float
            , c4 : Float
            }
ipfCoefficients m =
    case ( m.lift, m.gender ) of
        ( _, GNC ) ->
            Nothing

        ( Squat, Male ) ->
            Just { c1 = 123.1, c2 = 363.085, c3 = 25.1667, c4 = 75.4311 }

        ( Squat, Female ) ->
            Just { c1 = 50.479, c2 = 105.632, c3 = 19.1846, c4 = 56.2215 }

        ( Bench, Male ) ->
            Just { c1 = 86.4745, c2 = 259.155, c3 = 17.5785, c4 = 53.122 }

        ( Bench, Female ) ->
            Just { c1 = 25.0485, c2 = 43.848, c3 = 6.7172, c4 = 13.952 }

        ( Deadlift, Male ) ->
            Just { c1 = 103.5355, c2 = 244.765, c3 = 15.3714, c4 = 31.5022 }

        ( Deadlift, Female ) ->
            Just { c1 = 47.136, c2 = 67.349, c3 = 9.1555, c4 = 13.67 }

        ( Total, Male ) ->
            Just { c1 = 310.67, c2 = 857.785, c3 = 53.216, c4 = 147.0835 }

        ( Total, Female ) ->
            Just { c1 = 125.1435, c2 = 228.03, c3 = 34.5246, c4 = 86.8301 }



-- Wilks


wilksCoefficients : Feat -> Maybe (List Float)
wilksCoefficients m =
    case m.gender of
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


polynomialMultiply : Feat -> Int -> Float -> Float
polynomialMultiply m index const =
    const * m.bodyKilos ^ toFloat index


wilks : Feat -> Score
wilks m =
    case wilksCoefficients m of
        Just coefficients ->
            coefficients
                |> List.indexedMap (polynomialMultiply m)
                |> List.foldl (+) 0
                |> (/) (m.liftedKilos * 500)
                |> Wilks

        Nothing ->
            NoScore



-- McCulloch


mcCulloch : Feat -> Score
mcCulloch feat =
    case ( Maybe.map mcCullochFactor feat.age, wilks feat ) of
        ( Just scale, Wilks score ) ->
            score * scale |> McCulloch

        ( _, _ ) ->
            NoScore


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
