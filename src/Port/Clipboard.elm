module Port.Clipboard exposing (read)

import Json.Decode as Decode
import Platform exposing (Task)
import Task
import TaskPort



-- PORT CALLS


read : Task Never String
read =
    TaskPort.callNoArgs
        { function = "readClipboard"
        , valueDecoder = Decode.string
        }
        |> Task.onError (\_ -> Task.succeed "")
