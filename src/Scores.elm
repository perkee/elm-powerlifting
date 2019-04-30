module Scores exposing (Scores, scores, scoresToString)

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
    }


type alias Scores =
    Maybe ActualScores


scores : ModelInKilos -> Scores
scores m =
    case m of
        Just model ->
            Just
                { bodyKilos = model.bodyMass
                , liftedKilos = model.liftedMass
                , wilks = wilks model
                , allometric = allometric model
                }

        Nothing ->
            Nothing


scoresToString : Scores -> String
scoresToString s =
    case s of
        Just sc ->
            String.join ", "
                [ sc
                    |> .wilks
                    |> String.fromFloat
                    |> (++) "wilks: "
                , sc
                    |> .allometric
                    |> String.fromFloat
                    |> (++) "allometric: "
                ]

        Nothing ->
            "you gotta type stuff"



-- Allometric Scaling Scor


allometric : ActualModelInKilos -> Float
allometric m =
    m.bodyMass ^ (-2 / 3) * m.liftedMass



-- Wilks


coefficients : Sex -> List Float
coefficients sex =
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
        |> coefficients
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
                           |> coefficients
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
