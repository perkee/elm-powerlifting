module Mass exposing
    ( Mass
    , MassUnit(..)
    , decode
    , fromUnitAndFloat
    , max
    , serialize
    , sum
    , toKilos
    , toPounds
    , toUnitAndFloat
    , toggleMassUnit
    , zero
    )

import Json.Decode as D
import Json.Encode as E


type MassUnit
    = KG
    | LBM
    | Zero


type Mass
    = KgMass Float
    | LbmMass Float
    | ZeroMass


zero : Mass
zero =
    ZeroMass


add : Mass -> Mass -> Mass
add a b =
    case ( a, b ) of
        ( ZeroMass, bb ) ->
            bb

        ( aa, ZeroMass ) ->
            aa

        ( KgMass aa, KgMass bb ) ->
            KgMass <| aa + bb

        ( LbmMass aa, LbmMass bb ) ->
            LbmMass <| aa + bb

        ( LbmMass _, KgMass bb ) ->
            KgMass (bb + toKilos a)

        ( KgMass aa, LbmMass _ ) ->
            KgMass (aa + toKilos b)


sum : List Mass -> Mass
sum =
    List.foldl add ZeroMass


serializeStringAndFloat : ( String, Float ) -> E.Value
serializeStringAndFloat ( unit, number ) =
    E.object
        [ ( "number", E.float number )
        , ( "unit", E.string unit )
        ]


serialize : Mass -> E.Value
serialize m =
    case m of
        KgMass f ->
            serializeStringAndFloat ( "KG", f )

        LbmMass f ->
            serializeStringAndFloat ( "LBM", f )

        ZeroMass ->
            serializeStringAndFloat ( "Z", 0.0 )


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

        "Z" ->
            D.succeed Zero

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

        ZeroMass ->
            0


toPounds : Mass -> Float
toPounds m =
    case m of
        KgMass f ->
            f / poundsPerKilo

        LbmMass f ->
            f

        ZeroMass ->
            0


fromUnitAndFloat : MassUnit -> Float -> Mass
fromUnitAndFloat u f =
    case u of
        Zero ->
            ZeroMass

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

        ZeroMass ->
            ( Zero, 0 )


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

        Zero ->
            Zero
