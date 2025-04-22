module OutMsg exposing (..)


type OutMsg rootMsg
    = None
    | Single rootMsg
    | Multiple (List rootMsg)


toList : OutMsg rootMsg -> List rootMsg
toList outMsg =
    case outMsg of
        None ->
            []

        Single rootMsg ->
            [ rootMsg ]

        Multiple rootMsgs ->
            rootMsgs


combine : OutMsg rootMsg -> OutMsg rootMsg -> OutMsg rootMsg
combine a b =
    Multiple (toList a ++ toList b)
