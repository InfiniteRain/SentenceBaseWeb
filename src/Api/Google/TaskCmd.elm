module Api.Google.TaskCmd exposing (TaskCmd, attempt, cmd)

import Platform exposing (Task)
import Task



-- TYPES


type TaskCmd msg
    = TaskCmd (Cmd msg)



-- CONSTRUCTORS


attempt : (Result error payload -> msg) -> Task error payload -> TaskCmd msg
attempt toMsg task =
    TaskCmd <| Task.attempt toMsg task



-- TRANSFORMERS


cmd : TaskCmd msg -> Cmd msg
cmd (TaskCmd msg) =
    msg
