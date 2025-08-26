module Port.Timeout exposing (set)

import Json.Decode as Decode
import Json.Encode as Encode
import Platform exposing (Task)
import Task
import TaskPort



-- PORT CALLS


set : Int -> Int -> Task Never Int
set id time =
    TaskPort.call
        { function = "timeout"
        , valueDecoder = Decode.int
        , argsEncoder =
            \() ->
                Encode.object
                    [ ( "id", Encode.int id )
                    , ( "timeout", Encode.int time )
                    ]
        }
        ()
        |> Task.onError (\_ -> Task.succeed 0)
