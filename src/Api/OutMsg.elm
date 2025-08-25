module Api.OutMsg exposing (OutMsg, combine, none, some, toList)

import Effect exposing (Effect)



-- TYPES


type OutMsg rootMsg
    = None
    | Single (Effect rootMsg) rootMsg
    | Multiple (List ( Effect rootMsg, rootMsg ))



-- CONSTRUCTORS


none : OutMsg rootMsg
none =
    None


some : Effect rootMsg -> rootMsg -> OutMsg rootMsg
some =
    Single



-- ACCESSORS


toList : OutMsg rootMsg -> List ( Effect rootMsg, rootMsg )
toList outMsg =
    case outMsg of
        None ->
            []

        Single effect rootMsg ->
            [ ( effect, rootMsg ) ]

        Multiple rootMsgs ->
            rootMsgs



-- TRANSFORMERS


combine : OutMsg rootMsg -> OutMsg rootMsg -> OutMsg rootMsg
combine a b =
    Multiple (toList a ++ toList b)
