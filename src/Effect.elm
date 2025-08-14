module Effect exposing
    ( Effect
    , google
    , googleInitialize
    , map
    , match
    , none
    , toast
    , uuid
    , wiktionary
    )

import Api.Google as Google exposing (Action(..))
import Api.Google.Exchange.SheetsCmd as SheetsCmd exposing (SheetsCmd)
import Api.Wiktionary as Wiktionary
import Http
import Toast



-- TYPES


type Effect msg
    = None
    | Google (Google.Action msg)
    | Wiktionary (Wiktionary.RequestConfig msg)
    | Uuid (String -> msg)
    | Toast Toast.Config



-- CONSTRUCTORS


none : Effect msg
none =
    None


google : SheetsCmd msg -> Effect msg
google sheetsCmd =
    Google <| SendRequest sheetsCmd


googleInitialize : (Google.InitializeUpdate -> msg) -> Effect msg
googleInitialize msg =
    Google <| Initialize msg


wiktionary :
    (Result Http.Error Wiktionary.Usages -> msg)
    -> String
    -> Effect msg
wiktionary toMsg word =
    Wiktionary <| { word = word, toMsg = toMsg }


uuid : (String -> msg) -> Effect msg
uuid toMsg =
    Uuid toMsg


toast : Toast.Config -> Effect msg
toast config =
    Toast config



-- ACCESSORS


match :
    Effect msg
    ->
        { onNone : a
        , onGoogle : Google.Action msg -> a
        , onWiktionary : Wiktionary.RequestConfig msg -> a
        , onUuid : (String -> msg) -> a
        , onToast : Toast.Config -> a
        }
    -> a
match effect { onNone, onGoogle, onWiktionary, onUuid, onToast } =
    case effect of
        None ->
            onNone

        Google googleAction ->
            onGoogle googleAction

        Wiktionary request ->
            onWiktionary request

        Uuid toMsg ->
            onUuid toMsg

        Toast config ->
            onToast config



-- TRANSFORMERS


map : (a -> msg) -> Effect a -> Effect msg
map toMsg msg =
    case msg of
        None ->
            None

        Google (Initialize rootMsg) ->
            Google <| Initialize <| (toMsg << rootMsg)

        Google (SendRequest cmd) ->
            Google <| SendRequest <| SheetsCmd.map toMsg cmd

        Wiktionary request ->
            Wiktionary
                { word = request.word
                , toMsg = request.toMsg >> toMsg
                }

        Uuid uuidToMsg ->
            Uuid (uuidToMsg >> toMsg)

        Toast config ->
            Toast config
