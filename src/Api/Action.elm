module Api.Action exposing
    ( Action
    , google
    , googleInitialize
    , map
    , match
    , none
    , uuid
    , wiktionary
    )

import Api.Google as Google exposing (Action(..))
import Api.Google.Exchange.SheetsCmd as SheetsCmd exposing (SheetsCmd)
import Api.Wiktionary as Wiktionary
import Http



-- TYPES


type Action msg
    = None
    | Google (Google.Action msg)
    | Wiktionary (Wiktionary.RequestConfig msg)
    | Uuid (String -> msg)



-- CONSTRUCTORS


none : Action msg
none =
    None


google : SheetsCmd msg -> Action msg
google paramCmd =
    Google <| SendRequest paramCmd


googleInitialize : (Google.InitializeUpdate -> msg) -> Action msg
googleInitialize msg =
    Google <| Initialize msg


wiktionary :
    (Result Http.Error Wiktionary.Usages -> msg)
    -> String
    -> Action msg
wiktionary toMsg word =
    Wiktionary <| { word = word, toMsg = toMsg }


uuid : (String -> msg) -> Action msg
uuid toMsg =
    Uuid toMsg



-- ACCESSORS


match :
    Action msg
    ->
        { onNone : a
        , onGoogle : Google.Action msg -> a
        , onWiktionary : Wiktionary.RequestConfig msg -> a
        , onUuid : (String -> msg) -> a
        }
    -> a
match action { onNone, onGoogle, onWiktionary, onUuid } =
    case action of
        None ->
            onNone

        Google googleAction ->
            onGoogle googleAction

        Wiktionary request ->
            onWiktionary request

        Uuid toMsg ->
            onUuid toMsg



-- TRANSFORMERS


map : (a -> msg) -> Action a -> Action msg
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
