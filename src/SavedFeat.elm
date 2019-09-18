module SavedFeat exposing (SavedFeat)

import Feat exposing (Feat)


type alias SavedFeat =
    { feat : Feat
    , index : Int
    , key : Int
    }
