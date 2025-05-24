module Api.Google.ParamCmd exposing (ParamCmd, map)

-- TYPES


type alias ParamCmd msg =
    String -> String -> Cmd msg



-- HELPERS


map : (a -> msg) -> ParamCmd a -> ParamCmd msg
map toMsg paramCmd =
    \token sheetId ->
        Cmd.map toMsg <| paramCmd token sheetId
