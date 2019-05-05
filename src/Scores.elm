module Scores exposing
    ( Record
    , featToRecord
    , featToScores
    , recordToPara
    , recordToString
    , recordToTable
    , recordToText
    )

import Feat
    exposing
        ( Feat
        , Gender(..)
        , Lift(..)
        , MassUnit
        )
import Html as H exposing (Html)
import Renderer exposing (rowsToHeadedTable, textual)



-- Scores


type alias ScoreFn =
    Feat -> Float


type alias Scores =
    { wilks : Float
    , allometric : Float
    , ipf : Float
    }


type alias Record =
    { feat : Feat
    , scores : Scores
    }


recordToTableRows : Record -> List (Html msg)
recordToTableRows record =
    [ featToStatsList record.feat
    , featToMassesList record.feat
    , scoresToScoreList record.scores
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
            >> scoresToScoreList
            >> listToPara
        )
        >> Maybe.withDefault (H.div [] [ H.text "Cannot make a paragraph" ])


recordToText : Maybe Record -> Html msg
recordToText =
    Maybe.map recordToString
        >> Maybe.withDefault "Cannot make a string"
        >> H.text


featToRecord : Maybe Feat -> Maybe Record
featToRecord mf =
    case mf of
        Just feat ->
            Just
                { feat = feat
                , scores = featToScores feat
                }

        Nothing ->
            Nothing


featToScores : Feat -> Scores
featToScores feat =
    { wilks = wilks feat
    , allometric = allometric feat
    , ipf = ipf feat
    }


scoresToScoreList : Scores -> List ( String, String )
scoresToScoreList scores =
    [ ( .wilks, "Wilks" )
    , ( .allometric, "Allometric" )
    , ( .ipf, "IPF" )
    ]
        |> List.map
            (\( getter, label ) ->
                scores
                    |> getter
                    |> floatToString
                    |> (\score -> ( label, score ))
            )


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


recordToString : Record -> String
recordToString record =
    [ ( .wilks, "Wilks" )
    , ( .allometric, "Allometric" )
    , ( .ipf, "IPF" )
    ]
        |> List.map
            (\( getter, label ) ->
                record.scores
                    |> getter
                    |> floatToString
                    |> (++) (label ++ ": ")
            )
        |> String.join ", "
        |> (++)
            (floatToString record.feat.liftedKilos
                ++ " @ "
                ++ floatToString record.feat.bodyKilos
                ++ unitSeparatorSpace
                ++ "kg ("
                ++ floatToString record.feat.liftedPounds
                ++ " @ "
                ++ floatToString record.feat.bodyPounds
                ++ unitSeparatorSpace
                ++ "lb) = "
            )



-- Allometric Scaling Score


allometricCoefficient : Feat -> Float
allometricCoefficient m =
    case ( m.lift, m.gender ) of
        ( Squat, Male ) ->
            6.487682129

        ( Squat, Female ) ->
            8.540082411

        ( Bench, Male ) ->
            8.373410442

        ( Bench, Female ) ->
            11.26896531

        ( Deadlift, Male ) ->
            5.510559406

        ( Deadlift, Female ) ->
            7.164206454

        ( Total, Male ) ->
            2.292801981

        ( Total, Female ) ->
            3.195981761


allometric : Feat -> Float
allometric m =
    m.bodyKilos
        ^ (-2 / 3)
        * m.liftedKilos
        * allometricCoefficient m



-- IPF


ipf : Feat -> Float
ipf m =
    if abs m.liftedKilos < 0.25 then
        0

    else
        let
            cs =
                ipfCoefficients m

            scale =
                m.bodyKilos |> logBase e |> (*)
        in
        500
            + 100
            * (m.liftedKilos - (scale cs.c1 - cs.c2))
            / (scale cs.c3 - cs.c4)


ipfCoefficients :
    Feat
    ->
        { c1 : Float
        , c2 : Float
        , c3 : Float
        , c4 : Float
        }
ipfCoefficients m =
    case ( m.lift, m.gender ) of
        ( Squat, Male ) ->
            { c1 = 123.1, c2 = 363.085, c3 = 25.1667, c4 = 75.4311 }

        ( Squat, Female ) ->
            { c1 = 50.479, c2 = 105.632, c3 = 19.1846, c4 = 56.2215 }

        ( Bench, Male ) ->
            { c1 = 86.4745, c2 = 259.155, c3 = 17.5785, c4 = 53.122 }

        ( Bench, Female ) ->
            { c1 = 25.0485, c2 = 43.848, c3 = 6.7172, c4 = 13.952 }

        ( Deadlift, Male ) ->
            { c1 = 103.5355, c2 = 244.765, c3 = 15.3714, c4 = 31.5022 }

        ( Deadlift, Female ) ->
            { c1 = 47.136, c2 = 67.349, c3 = 9.1555, c4 = 13.67 }

        ( Total, Male ) ->
            { c1 = 310.67, c2 = 857.785, c3 = 53.216, c4 = 147.0835 }

        ( Total, Female ) ->
            { c1 = 125.1435, c2 = 228.03, c3 = 34.5246, c4 = 86.8301 }



-- Wilks


wilksCoefficients : Feat -> List Float
wilksCoefficients m =
    case m.gender of
        Male ->
            [ -216.0475144
            , 16.2606339
            , -0.002388645
            , -0.00113732
            , 7.01863e-6
            , -1.291e-8
            ]

        Female ->
            [ 594.31747775582
            , -27.23842536447
            , 0.82112226871
            , -0.00930733913
            , 4.731582e-5
            , -9.054e-8
            ]


polynomialMultiply : Feat -> Int -> Float -> Float
polynomialMultiply m index const =
    const * m.bodyKilos ^ toFloat index


wilks : Feat -> Float
wilks m =
    m
        |> wilksCoefficients
        |> List.indexedMap (polynomialMultiply m)
        |> List.foldl (+) 0
        |> (/) (m.liftedKilos * 500)


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
