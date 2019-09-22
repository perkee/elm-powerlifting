module SavedFeat exposing (SavedFeat, maxRecord)

import Feat exposing (Feat)
import Scores
    exposing
        ( Record
        , featToRecord
        )


type alias SavedFeat =
    { feat : Feat
    , index : Int
    , key : Int
    }


maxRecord : List SavedFeat -> Record
maxRecord =
    List.map (.feat >> featToRecord) >> Scores.maxRecord
