module Api exposing (Action(..), mapAction)

import Api.Google as Google
import Api.Wiktionary as Wiktionary


type Action msg
    = None
    | Google (Google.RequestConfig msg)
    | Wiktionary (Wiktionary.RequestConfig msg)


mapAction : (a -> msg) -> Action a -> Action msg
mapAction map msg =
    case msg of
        None ->
            None

        Google request ->
            Google
                { method = request.method
                , url = request.url
                , body = request.body
                , expect = Google.mapExpect map request.expect
                }

        Wiktionary request ->
            Wiktionary
                { word = request.word
                , toMsg = \result -> map (request.toMsg result)
                }
