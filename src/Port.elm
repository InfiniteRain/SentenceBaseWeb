module Port exposing
    ( googleGetToken
    , googleGetTokenRefresh
    , googleInitialize
    , localStorageGet
    , localStorageRemove
    , localStorageSet
    , readClipboard
    , timeout
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import TaskPort



-- GOOGLE INITIALIZE


googleInitialize : TaskPort.Task ()
googleInitialize =
    TaskPort.callNoArgs
        { function = "googleInitialize"
        , valueDecoder = Decode.succeed ()
        }



-- GOOGLE AUTHENTICATE


googleGetToken : TaskPort.Task String
googleGetToken =
    googleGetTokenInner False


googleGetTokenRefresh : TaskPort.Task String
googleGetTokenRefresh =
    googleGetTokenInner True


googleGetTokenInner : Bool -> TaskPort.Task String
googleGetTokenInner =
    TaskPort.call
        { function = "googleGetToken"
        , valueDecoder = Decode.string
        , argsEncoder = Encode.bool
        }



-- CLIPBOARD


readClipboard : TaskPort.Task String
readClipboard =
    TaskPort.callNoArgs
        { function = "readClipboard"
        , valueDecoder = Decode.string
        }



-- TIMEOUT


timeout : Int -> Int -> TaskPort.Task Int
timeout id time =
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



-- LOCAL STORAGE


localStorageSet : String -> Encode.Value -> TaskPort.Task ()
localStorageSet key =
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


localStorageGet : String -> Decoder a -> TaskPort.Task a
localStorageGet key decoder =
    TaskPort.call
        { function = "localStorageGet"
        , valueDecoder = decoder
        , argsEncoder = Encode.string
        }
        key


localStorageRemove : String -> TaskPort.Task ()
localStorageRemove key =
    TaskPort.call
        { function = "localStorageRemove"
        , valueDecoder = Decode.succeed ()
        , argsEncoder = Encode.string
        }
        key
