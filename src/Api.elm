module Api exposing (..)

import Api.Google as Google


type Action msg
    = None
    | Google (Google.RequestConfig msg)


mapAction : (a -> msg) -> Action a -> Action msg
mapAction map msg =
    case msg of
        Google body ->
            Google
                { method = body.method
                , url = body.url
                , body = body.body
                , expect = Google.mapExpect map body.expect
                }

        None ->
            None
