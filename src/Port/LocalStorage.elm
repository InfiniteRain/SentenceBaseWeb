module Port.LocalStorage exposing (..)

import Json.Decode as Decode exposing (Decoder, Error(..))
import Json.Encode as Encode
import Platform exposing (Task)
import Task
import TaskPort exposing (InteropError(..))



-- PORT CALLS


set : Encode.Value -> String -> Task Never ()
set encoder key =
    TaskPort.call
        { function = "localStorageSet"
        , valueDecoder = Decode.succeed ()
        , argsEncoder =
            \value ->
                Encode.object
                    [ ( "key", Encode.string key )
                    , ( "value", value )
                    ]
        }
        encoder
        |> Task.onError (\_ -> Task.succeed ())


get : Decoder a -> String -> Task Decode.Error a
get decoder =
    TaskPort.call
        { function = "localStorageGet"
        , valueDecoder = decoder
        , argsEncoder = Encode.string
        }
        >> Task.onError
            (\err ->
                case err of
                    TaskPort.InteropError (CannotDecodeValue error _) ->
                        Task.fail error

                    _ ->
                        Task.fail <| Decode.Failure "" (Encode.int 0)
            )


remove : String -> Task Never ()
remove =
    TaskPort.call
        { function = "localStorageRemove"
        , valueDecoder = Decode.succeed ()
        , argsEncoder = Encode.string
        }
        >> Task.onError (\_ -> Task.succeed ())
