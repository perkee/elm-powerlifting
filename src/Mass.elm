module Mass exposing
    ( Mass
    , MassUnit(..)
    , fromUnitAndFloat
    , max
    , serialize
    , decode
    , toKilos
    , toPounds
    , toUnitAndFloat
    , toggleMassUnit
    )

import Json.Decode as D
import Json.Encode as E


type MassUnit
    = KG
    | LBM


type Mass
    = KgMass Float
    | LbmMass Float


serialize : Mass -> E.Value
serialize m =
    let
        ( unit, number ) =
            case m of
                KgMass f ->
                    ( "KG", f )

                LbmMass f ->
                    ( "LBM", f )
    in
    E.object
        [ ( "number", E.float number )
        , ( "unit", E.string unit )
        ]


decode : D.Decoder Mass
decode =
    D.map2 fromUnitAndFloat
        (D.field "unit" (D.string |> D.andThen stringToMaybeUnit))
        (D.field "number" D.float)


stringToMaybeUnit : String -> D.Decoder MassUnit
stringToMaybeUnit s =
    case s of
        "KG" ->
            D.succeed KG

        "LBM" ->
            D.succeed LBM

        _ ->
            D.fail ("unknown MassUnit " ++ s)


poundsPerKilo : Float
poundsPerKilo =
    0.45359237


toKilos : Mass -> Float
toKilos m =
    case m of
        KgMass f ->
            f

        LbmMass f ->
            f * poundsPerKilo


toPounds : Mass -> Float
toPounds m =
    case m of
        KgMass f ->
            f / poundsPerKilo

        LbmMass f ->
            f


fromUnitAndFloat : MassUnit -> Float -> Mass
fromUnitAndFloat u f =
    case u of
        KG ->
            KgMass f

        LBM ->
            LbmMass f


toUnitAndFloat : Mass -> ( MassUnit, Float )
toUnitAndFloat mass =
    case mass of
        KgMass f ->
            ( KG, f )

        LbmMass f ->
            ( LBM, f )


compare : Mass -> Mass -> Order
compare a b =
    case ( a, b ) of
        ( LbmMass la, LbmMass lb ) ->
            Basics.compare la lb

        ( _, _ ) ->
            Basics.compare (toKilos a) (toKilos b)


max : Mass -> Mass -> Mass
max a b =
    case compare a b of
        GT ->
            a

        LT ->
            b

        EQ ->
            a


toggleMassUnit : MassUnit -> MassUnit
toggleMassUnit unit =
    case unit of
        KG ->
            LBM

        LBM ->
            KG
