module Port exposing
    ( googleGetToken
    , googleGetTokenRefresh
    , googleInitialize
    , readClipboard
    , timeout
    )

import Json.Decode as Decode
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
