module OutMsg exposing (OutMsg, combine, none, some, toList)

-- TYPES


type OutMsg rootMsg
    = None
    | Single rootMsg
    | Multiple (List rootMsg)



-- CONSTRUCTORS


none : OutMsg rootMsg
none =
    None


some : rootMsg -> OutMsg rootMsg
some rootMsg =
    Single rootMsg



-- ACCESSORS


toList : OutMsg rootMsg -> List rootMsg
toList outMsg =
    case outMsg of
        None ->
            []

        Single rootMsg ->
            [ rootMsg ]

        Multiple rootMsgs ->
            rootMsgs



-- TRANSFORMERS


combine : OutMsg rootMsg -> OutMsg rootMsg -> OutMsg rootMsg
combine a b =
    Multiple (toList a ++ toList b)
