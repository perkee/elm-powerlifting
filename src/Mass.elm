module Mass exposing
    ( Mass
    , MassUnit(..)
    , fromUnitAndFloat
    , max
    , toKilos
    , toPounds
    , toUnitAndFloat
    , toggleMassUnit
    )


type MassUnit
    = KG
    | LBM


type Mass
    = KgMass Float
    | LbmMass Float


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
