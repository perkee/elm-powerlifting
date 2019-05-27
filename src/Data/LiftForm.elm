module Data.LiftForm exposing (State, init, subscriptions, toFeat)

import Data.UnitDropdown as UnitDropdown
import Feat exposing (Equipment(..), Feat, Gender(..), Lift(..), MassUnit(..))
import Platform.Sub


type alias State =
    { liftedMass : String
    , liftedUnit : UnitDropdown.State
    , bodyMass : String
    , bodyUnit : UnitDropdown.State
    , gender : Gender
    , lift : Lift
    , age : String
    , equipment : Equipment
    }


subscriptions : State -> (State -> msg) -> Platform.Sub.Sub msg
subscriptions state updateMsg =
    Sub.batch
        [ UnitDropdown.subscriptions state.liftedUnit
            ((\x -> { state | liftedUnit = x })
                >> updateMsg
            )
        , UnitDropdown.subscriptions state.bodyUnit
            (updateMsg << (\x -> { state | bodyUnit = x }))
        ]


init : State
init =
    { liftedMass = ""
    , liftedUnit = UnitDropdown.init
    , bodyMass = ""
    , bodyUnit = UnitDropdown.init
    , gender = GNC
    , lift = Total
    , age = ""
    , equipment = Raw
    }


toFeat : State -> Maybe Feat
toFeat state =
    case ( String.toFloat state.bodyMass, String.toFloat state.liftedMass ) of
        ( Just bodyMass, Just liftedMass ) ->
            Just
                { bodyKilos = UnitDropdown.toKilos state.bodyUnit bodyMass
                , bodyPounds = UnitDropdown.toPounds state.bodyUnit bodyMass
                , liftedKilos = UnitDropdown.toKilos state.liftedUnit liftedMass
                , liftedPounds = UnitDropdown.toPounds state.liftedUnit liftedMass
                , gender = state.gender
                , lift = state.lift
                , age = String.toFloat state.age
                , equipment = state.equipment
                }

        ( _, _ ) ->
            Nothing
