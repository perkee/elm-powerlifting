module Scores exposing
    ( Record
    , featToRecord
    , featToScores
    , featToString
    , recordToCells
    , recordToPara
    , recordToTable
    )

import Array
import Feat
    exposing
        ( Feat
        , Gender(..)
        , Lift(..)
        , MassUnit
        )
import Html as H exposing (Html)
import Library exposing (thrush)
import Renderer exposing (rowsToHeadedTable, textual)



-- Scores


type Score
    = Wilks Float
    | ScaledAllometric Float
    | Ipf Float
    | McCulloch Float
    | Allometric Float
    | NoScore


type alias Scores =
    List Score


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


maybeFloatToString : Maybe Float -> String
maybeFloatToString f =
    case f of
        Just float ->
            floatToString float

        Nothing ->
            "—"


recordToCells : Record -> List (Html msg)
recordToCells record =
    [ [ .feat >> .liftedKilos
      , .feat >> .bodyKilos
      , .feat >> .liftedPounds
      , .feat >> .bodyPounds
      ]
        |> List.map (thrush record >> floatToString >> H.text)
    , [ .wilks
      , .scaledAllometric
      , .allometric
      , .ipf
      , .mcCulloch
      ]
        |> List.map (thrush record >> maybeFloatToString >> H.text)
    ]
        |> List.concat


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


scoreToPair : Score -> Maybe ( String, String )
scoreToPair score =
    case score of
        Wilks s ->
            Just ( "Wilks", floatToString s )

        ScaledAllometric s ->
            Just ( "Scaled Allometric", floatToString s )

        Allometric s ->
            Just ( "Allometric", floatToString s )

        Ipf s ->
            Just ( "IPF", floatToString s )

        McCulloch s ->
            Just ( "McCulloch", floatToString s )

        NoScore ->
            Nothing


scoresToPairs : List Score -> List ( String, String )
scoresToPairs =
    List.foldr
        (\score pairs ->
            case scoreToPair score of
                Just pair ->
                    pair :: pairs

                Nothing ->
                    pairs
        )
        []


recordToTableRows : Record -> List (Html msg)
recordToTableRows record =
    [ featToStatsList record.feat
    , featToMassesList record.feat
    , record.feat |> featToScores |> scoresToPairs
    ]
        |> List.concat
        |> listToRows


recordToTable : Maybe Record -> Html msg
recordToTable mr =
    case mr of
        Just record ->
            record
                |> recordToTableRows
                |> rowsToHeadedTable [ "Label", "Score" ]

        Nothing ->
            H.div [] [ H.text "Cannot make a table" ]


recordToPara : Maybe Record -> Html msg
recordToPara =
    Maybe.map
        (.feat
            >> featToScores
            >> scoresToPairs
            >> listToPara
        )
        >> Maybe.withDefault (H.div [] [ H.text "Cannot make a paragraph" ])


featToScores : Feat -> List Score
featToScores feat =
    [ wilks
    , scaledAllometric
    , allometric
    , ipf
    , mcCulloch
    ]
        |> List.map (\fn -> fn feat)


featToMassesList : Feat -> List ( String, String )
featToMassesList feat =
    [ ( .bodyKilos, "Body mass (kg)" )
    , ( .bodyPounds, "Body mass (lb)" )
    , ( .liftedKilos, "Lifted mass (kg)" )
    , ( .liftedPounds, "Lifted mass (lb)" )
    ]
        |> List.map
            (\( getter, label ) ->
                feat
                    |> getter
                    |> floatToString
                    |> (\score -> ( label, score ))
            )


featToStatsList : Feat -> List ( String, String )
featToStatsList feat =
    [ ( "Gender"
      , case feat.gender of
            Male ->
                "Male"

            Female ->
                "Female"

            GNC ->
                "Other"
      )
    , ( "Lift"
      , case feat.lift of
            Squat ->
                "Squat"

            Bench ->
                "Bench"

            Deadlift ->
                "Deadlift"

            Total ->
                "Total"
      )
    ]


listToRows : List ( String, String ) -> List (Html msg)
listToRows =
    List.map
        (\( label, score ) ->
            H.tr []
                [ textual H.td label
                , textual H.td score
                ]
        )


listToTable : List ( String, String ) -> Html msg
listToTable list =
    list
        |> listToRows
        |> H.tbody []
        |> List.singleton
        |> (::)
            (H.thead
                []
                [ H.tr []
                    [ textual H.th "Label"
                    , textual H.th "Score"
                    ]
                ]
            )
        |> H.table []


listToPara : List ( String, String ) -> Html msg
listToPara list =
    List.map
        (\( label, score ) ->
            label ++ ": " ++ score
        )
        list
        |> String.join ", "
        |> H.text
        |> List.singleton
        |> H.div []


unitSeparatorSpace : String
unitSeparatorSpace =
    String.fromChar '\u{200A}'


featToString : Feat -> String
featToString feat =
    feat
        |> featToScores
        |> scoresToPairs
        |> List.map (\( label, score ) -> label ++ ": " ++ score)
        |> String.join ", "
        |> (++)
            (floatToString feat.liftedKilos
                ++ " @ "
                ++ floatToString feat.bodyKilos
                ++ unitSeparatorSpace
                ++ "kg ("
                ++ floatToString feat.liftedPounds
                ++ " @ "
                ++ floatToString feat.bodyPounds
                ++ unitSeparatorSpace
                ++ "lb) = "
            )



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


truncate : Int -> Float -> Float
truncate places n =
    let
        factor =
            10.0 ^ toFloat places
    in
    n
        |> (*) factor
        |> round
        |> toFloat
        |> (\m -> m / factor)


floatToString : Float -> String
floatToString =
    truncate 2 >> String.fromFloat



-- Nuckols


nuckols : Feat -> Float
nuckols m =
    0



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
