module Api exposing (Action(..), mapAction)

import Api.Google.Requests as GoogleRequests
import Api.Wiktionary as Wiktionary


type Action msg
    = None
    | Google (GoogleRequests.Action msg)
    | Wiktionary (Wiktionary.RequestConfig msg)


mapAction : (a -> msg) -> Action a -> Action msg
mapAction map msg =
    case msg of
        None ->
            None

        Google (GoogleRequests.Initialize rootMsg) ->
            Google (GoogleRequests.Initialize <| map rootMsg)

        Google (GoogleRequests.SendRequest request) ->
            Google <|
                GoogleRequests.SendRequest
                    { method = request.method
                    , url = request.url
                    , body = request.body
                    , expect = GoogleRequests.mapExpect map request.expect
                    }

        Wiktionary request ->
            Wiktionary
                { word = request.word
                , toMsg = \result -> map <| request.toMsg result
                }
