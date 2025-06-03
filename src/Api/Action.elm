module Api.Action exposing
    ( Action
    , google
    , googleInitialize
    , map
    , match
    , none
    , wiktionary
    )

import Api.Google as Google exposing (Action(..))
import Api.Google.ParamCmd as ParamCmd exposing (ParamCmd)
import Api.Wiktionary as Wiktionary
import Http



-- TYPES


type Action msg
    = None
    | Google (Google.Action msg)
    | Wiktionary (Wiktionary.RequestConfig msg)



-- CONSTRUCTORS


none : Action msg
none =
    None


google : ParamCmd msg -> Action msg
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



-- ACCESSORS


match :
    Action msg
    ->
        { onNone : a
        , onGoogle : Google.Action msg -> a
        , onWiktionary : Wiktionary.RequestConfig msg -> a
        }
    -> a
match action { onNone, onGoogle, onWiktionary } =
    case action of
        None ->
            onNone

        Google googleAction ->
            onGoogle googleAction

        Wiktionary request ->
            onWiktionary request



-- TRANSFORMERS


map : (a -> msg) -> Action a -> Action msg
map toMsg msg =
    case msg of
        None ->
            None

        Google (Initialize rootMsg) ->
            Google <| Initialize <| (toMsg << rootMsg)

        Google (SendRequest paramCmd) ->
            Google <| SendRequest <| ParamCmd.map toMsg paramCmd

        Wiktionary request ->
            Wiktionary
                { word = request.word
                , toMsg = request.toMsg >> toMsg
                }
