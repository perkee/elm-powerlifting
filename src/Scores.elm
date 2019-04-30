module Scores exposing (Scores, scores, scoresToPara, scoresToTable)

import Html as H exposing (Html)
import ModelInKilos
    exposing
        ( ActualModelInKilos
        , MassUnit
        , ModelInKilos
        , Sex(..)
        )



-- Scores


type alias ActualScores =
    { bodyKilos : Float
    , liftedKilos : Float
    , wilks : Float
    , allometric : Float
    , ipfRawTotal : Float
    }


type alias Scores =
    Maybe
        { bodyKilos : Float
        , liftedKilos : Float
        , wilks : Float
        , allometric : Float
        , ipfRawTotal : Float
        }


scoresToTable : ModelInKilos -> Html msg
scoresToTable =
    scores
        >> scoresToList
        >> listToTable


scoresToPara : ModelInKilos -> Html msg
scoresToPara =
    scores
        >> scoresToList
        >> listToPara


scores : ModelInKilos -> Scores
scores m =
    case m of
        Just model ->
            Just
                { bodyKilos = model.bodyMass
                , liftedKilos = model.liftedMass
                , wilks = wilks model
                , allometric = allometric model
                , ipfRawTotal = ipfRawTotal model
                }

        Nothing ->
            Nothing


scoresToList : Scores -> Maybe (List ( String, String ))
scoresToList s =
    case s of
        Just sc ->
            [ ( .wilks, "Wilks" )
            , ( .allometric, "Allometric" )
            , ( .ipfRawTotal, "IPF (raw total)" )
            ]
                |> List.map
                    (\( getter, label ) ->
                        sc
                            |> getter
                            |> String.fromFloat
                            |> (\score -> ( label, score ))
                    )
                |> Just

        Nothing ->
            Nothing


listToTable : Maybe (List ( String, String )) -> Html msg
listToTable ml =
    case ml of
        Just l ->
            List.map
                (\( label, score ) ->
                    H.tr []
                        [ H.td [] [ H.text label ]
                        , H.td [] [ H.text score ]
                        ]
                )
                l
                |> H.tbody []
                |> List.singleton
                |> H.table []

        Nothing ->
            H.div [] [ H.text "Cannot do" ]


listToPara : Maybe (List ( String, String )) -> Html msg
listToPara ml =
    case ml of
        Just l ->
            List.map
                (\( label, score ) ->
                    label ++ ": " ++ score
                )
                l
                |> String.join ", "
                |> H.text
                |> List.singleton
                |> H.div []

        Nothing ->
            H.div [] [ H.text "Cannot do" ]


scoresToString : Scores -> String
scoresToString s =
    case s of
        Just sc ->
            [ ( .wilks, "Wilks" )
            , ( .allometric, "Allometric" )
            , ( .ipfRawTotal, "IPF (raw total)" )
            ]
                |> List.map
                    (\( getter, label ) ->
                        sc
                            |> getter
                            |> String.fromFloat
                            |> (++) (label ++ ": ")
                    )
                |> String.join ", "

        Nothing ->
            "you gotta type stuff"



-- Allometric Scaling Score


allometric : ActualModelInKilos -> Float
allometric m =
    m.bodyMass ^ (-2 / 3) * m.liftedMass



-- IPF


ipfRawTotal : ActualModelInKilos -> Float
ipfRawTotal m =
    if abs m.liftedMass < 0.25 then
        0

    else
        let
            cs =
                ipfCoefficients m.sex

            scale =
                m.bodyMass |> logBase e |> (*)
        in
        500
            + 100
            * (m.liftedMass - (scale cs.c1 - cs.c2))
            / (scale cs.c3 - cs.c4)


ipfCoefficients :
    Sex
    ->
        { c1 : Float
        , c2 : Float
        , c3 : Float
        , c4 : Float
        }
ipfCoefficients sex =
    case sex of
        Male ->
            { c1 = 310.67
            , c2 = 857.785
            , c3 = 53.216
            , c4 = 147.0835
            }

        Female ->
            { c1 = 3125.1435
            , c2 = 228.03
            , c3 = 34.5246
            , c4 = 86.8301
            }



-- Wilks


wilksCoefficients : Sex -> List Float
wilksCoefficients sex =
    case sex of
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


polynomialMultiply : Float -> Int -> Float -> Float
polynomialMultiply base index const =
    const * base ^ toFloat index


wilks : ActualModelInKilos -> Float
wilks m =
    m.sex
        |> wilksCoefficients
        |> List.indexedMap (polynomialMultiply m.bodyMass)
        |> List.foldl (+) 0
        |> (\denom ->
                m.liftedMass
                    * 500
                    / denom
           )



{-
   deeplyNestedIrritatingWilks : Model -> String
   deeplyNestedIrritatingWilks m =
       case m.bodyMass.value of
           Just bodyMass ->
               case m.liftedMass.value of
                   Just lifted ->
                       m.sex
                           |> wilksCoefficients
                           |> List.indexedMap (mult bodyMass)
                           |> List.foldl (+) 0
                           |> (\denom ->
                                   lifted
                                       * 500
                                       / denom
                                       |> String.fromFloat
                              )

                   Nothing ->
                       "no lifted!"

           Nothing ->
               "No Body mass"
-}
