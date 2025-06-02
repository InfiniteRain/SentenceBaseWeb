module Api.Google.ParamTask exposing (ParamTask, attempt)

import Api.Google.ParamCmd exposing (ParamCmd)
import Task exposing (Task)



-- TYPES


type alias ParamTask error value =
    String -> Task error value



-- HELPERS


attempt : (Result error value -> msg) -> ParamTask error value -> ParamCmd msg
attempt resultToMsg fn =
    \sheetId ->
        fn sheetId
            |> Task.attempt resultToMsg
