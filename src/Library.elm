module Library exposing (thrush, updateArrayAt)

import Array exposing (Array)


thrush : a -> (a -> b) -> b
thrush input fn =
    fn input


updateArrayAt : Int -> (a -> a) -> Array a -> Array a
updateArrayAt index fn array =
    case Array.get index array of
        Just value ->
            Array.set index (fn value) array

        Nothing ->
            array
