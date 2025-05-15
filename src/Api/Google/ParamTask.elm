module Api.Google.ParamTask exposing (ParamTask, andThen, attempt)

import Api.Google.ParamCmd exposing (ParamCmd)
import Task exposing (Task)


type alias ParamTask error value =
    String -> String -> Task error value


fromTask : Task error value -> ParamTask error value
fromTask task =
    \_ _ -> task


andThen : (a -> ParamTask error b) -> ParamTask error a -> ParamTask error b
andThen callback fnA =
    \token sheetId ->
        fnA token sheetId
            |> Task.andThen (\a -> callback a token sheetId)


attempt : (Result error value -> msg) -> ParamTask error value -> ParamCmd msg
attempt resultToMsg fn =
    \token sheetId ->
        fn token sheetId
            |> Task.attempt resultToMsg
