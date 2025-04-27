module Api.Google.TaskCmd exposing (TaskCmd, attempt, cmd)

import Platform exposing (Task)
import Task


type TaskCmd msg
    = TaskCmd (Cmd msg)


attempt : (Result error payload -> msg) -> Task error payload -> TaskCmd msg
attempt toMsg task =
    TaskCmd <| Task.attempt toMsg task


cmd : TaskCmd msg -> Cmd msg
cmd (TaskCmd msg) =
    msg
