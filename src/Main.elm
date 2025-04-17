port module Main exposing (..)

import Api exposing (GapiRequestConfig, Update(..))
import Browser
import Html exposing (Html)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Page.Mining as Mining



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }



-- PORTS


port messageSenderPort : Encode.Value -> Cmd msg


messageSender : OutgoingMsg -> Cmd msg
messageSender msg =
    msg |> portMsgEncoder |> messageSenderPort


port messageReceiverPort : (Decode.Value -> msg) -> Sub msg


messageReceiver : Sub ApiMsg
messageReceiver =
    messageReceiverPort (\value -> IncomingPortMsg (Decode.decodeValue portMsgDecoder value))



-- MODEL


type alias Model =
    { apiModel : ApiModel
    , page : Page
    }


type Page
    = Mining Mining.Model


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( miningModel, miningCmd ) =
            Mining.init ()
    in
    ( { apiModel =
            { state = Uninitialized
            , token = ""
            , requestQueue = []
            }
      , page = Mining miningModel
      }
    , Cmd.map GotMiningMsg miningCmd
    )



-- API MODEL


type alias ApiModel =
    { state : ApiState
    , token : String
    , requestQueue : List (Api.GapiRequestConfig Msg)
    }


type ApiState
    = Uninitialized
    | Initializing
    | Authenticating
    | Idle
    | Sending



-- UPDATE


type Msg
    = GotApiMsg ApiMsg
    | GotMiningMsg Mining.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( GotApiMsg subMsg, _ ) ->
            let
                ( newApiModel, apiCmd, maybeOutMsg ) =
                    updateApi subMsg model.apiModel

                newModel =
                    { model | apiModel = newApiModel }

                res =
                    case maybeOutMsg of
                        Just outMsg ->
                            update outMsg newModel

                        Nothing ->
                            ( newModel, Cmd.none )
            in
            ( Tuple.first res
            , Cmd.batch [ Cmd.map GotApiMsg apiCmd, Tuple.second res ]
            )

        ( GotMiningMsg subMsg, Mining subModel ) ->
            let
                ( newMiningModel, apiUpdate ) =
                    Mining.update subMsg subModel

                newModel =
                    { model | page = Mining newMiningModel }
            in
            case apiUpdate of
                None ->
                    ( newModel
                    , Cmd.none
                    )

                Command cmd ->
                    ( newModel
                    , Cmd.map GotMiningMsg cmd
                    )

                Api action ->
                    let
                        ( newerModel, apiCmd ) =
                            Api.mapAction GotMiningMsg action
                                |> performApiAction newModel
                    in
                    ( newerModel, apiCmd )



-- API UPDATE


type ApiMsg
    = IncomingPortMsg (Result Decode.Error IncomingMsg)
    | SendGapiRequest (Api.GapiRequestConfig Msg)
    | ReceiveGapiResponse (Result Http.Error String)


updateApi : ApiMsg -> ApiModel -> ( ApiModel, Cmd ApiMsg, Maybe Msg )
updateApi msg model =
    case ( model.state, msg ) of
        ( Uninitialized, SendGapiRequest request ) ->
            ( { model
                | state = Initializing
                , requestQueue = request :: model.requestQueue
              }
            , messageSender InitializeRequest
            , Nothing
            )

        ( Initializing, SendGapiRequest request ) ->
            ( { model | requestQueue = model.requestQueue ++ [ request ] }
            , Cmd.none
            , Nothing
            )

        ( Authenticating, SendGapiRequest request ) ->
            ( { model | requestQueue = model.requestQueue ++ [ request ] }
            , Cmd.none
            , Nothing
            )

        ( Idle, SendGapiRequest request ) ->
            ( { model
                | state = Sending
                , requestQueue = model.requestQueue ++ [ request ]
              }
            , sendGapiRequest model.token request
            , Nothing
            )

        ( Sending, SendGapiRequest request ) ->
            ( { model | requestQueue = request :: model.requestQueue }
            , Cmd.none
            , Nothing
            )

        ( Sending, ReceiveGapiResponse response ) ->
            case model.requestQueue of
                request :: [] ->
                    ( { model
                        | state = Idle
                        , requestQueue = []
                      }
                    , Cmd.none
                    , Just (Api.decodeGapiExpect request.expect response)
                    )

                request :: rest ->
                    ( { model
                        | state = Sending
                        , requestQueue = rest
                      }
                    , sendGapiRequest model.token request
                    , Just (Api.decodeGapiExpect request.expect response)
                    )

                [] ->
                    ( { model | state = Idle }, Cmd.none, Nothing )

        ( unhandledState, ReceiveGapiResponse response ) ->
            let
                _ =
                    Debug.log
                        "received gapi response in a non-sending state"
                        ( unhandledState, response )
            in
            case model.requestQueue of
                request :: rest ->
                    ( { model | requestQueue = rest }
                    , Cmd.none
                    , Just (Api.decodeGapiExpect request.expect response)
                    )

                [] ->
                    ( model, Cmd.none, Nothing )

        ( _, IncomingPortMsg portMsg ) ->
            case portMsg of
                Ok decodedPortMsg ->
                    handleIncomingMsg decodedPortMsg model

                Err err ->
                    let
                        _ =
                            Debug.log "error decoding port msg" (Decode.errorToString err)
                    in
                    ( model
                    , Cmd.none
                    , Nothing
                    )


handleIncomingMsg : IncomingMsg -> ApiModel -> ( ApiModel, Cmd ApiMsg, Maybe Msg )
handleIncomingMsg msg model =
    case ( model.state, msg ) of
        ( Initializing, InitializedResponse ) ->
            ( { model | state = Authenticating }
            , messageSender AuthenticateRequest
            , Nothing
            )

        ( Initializing, InitializeFailedResponse err ) ->
            let
                _ =
                    Debug.log "initialization error" err
            in
            ( { model | state = Uninitialized }
            , Cmd.none
            , Nothing
            )

        ( Authenticating, AuthenticateResponse res ) ->
            case model.requestQueue of
                request :: _ ->
                    ( { model | token = res.token, state = Sending }
                    , sendGapiRequest res.token request
                    , Nothing
                    )

                [] ->
                    ( { model | token = res.token, state = Idle }
                    , Cmd.none
                    , Nothing
                    )

        ( Authenticating, AuthenticateFailedResponse err ) ->
            let
                _ =
                    Debug.log "authentication error" err
            in
            ( { model | state = Uninitialized }
            , Cmd.none
            , Nothing
            )

        ( Authenticating, InteractionRequiredResponse ) ->
            Debug.todo "Implement interactionRequiredResponse"

        unhandledCombination ->
            let
                _ =
                    Debug.log "unhandled state/msg combination in handleIncomingMsg" unhandledCombination
            in
            ( model
            , Cmd.none
            , Nothing
            )


performApiAction : Model -> Api.Action Msg -> ( Model, Cmd Msg )
performApiAction model action =
    case action of
        Api.Gapi request ->
            update (GotApiMsg (SendGapiRequest request)) model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.map GotApiMsg messageReceiver



-- VIEW


view : Model -> Html Msg
view model =
    case model.page of
        Mining subModel ->
            Html.map GotMiningMsg (Mining.view subModel)



-- OUTGOING MESSAGE


type OutgoingMsg
    = InitializeRequest
    | AuthenticateRequest


portMsgEncoder : OutgoingMsg -> Encode.Value
portMsgEncoder msg =
    case msg of
        InitializeRequest ->
            Encode.object [ ( "tag", Encode.string "initializeRequest" ) ]

        AuthenticateRequest ->
            Encode.object [ ( "tag", Encode.string "authenticateRequest" ) ]



-- INCOMING MESSAGE


type IncomingMsg
    = InitializedResponse
    | InitializeFailedResponse { message : String }
    | AuthenticateResponse { token : String }
    | AuthenticateFailedResponse { message : String }
    | InteractionRequiredResponse


portMsgDecoder : Decoder IncomingMsg
portMsgDecoder =
    Decode.field "tag" Decode.string
        |> Decode.andThen portMsgFromTag


portMsgFromTag : String -> Decoder IncomingMsg
portMsgFromTag str =
    case str of
        "initializedResponse" ->
            Decode.succeed InitializedResponse

        "initializeFailedResponse" ->
            initializeFailedDecoder

        "authenticateResponse" ->
            authenticateResponseDecoder

        "authenticateFailedResponse" ->
            authenticateFailedDecoder

        "interactionRequiredResponse" ->
            Decode.succeed InteractionRequiredResponse

        _ ->
            Decode.fail ("Invalid tag: " ++ str)


initializeFailedDecoder : Decoder IncomingMsg
initializeFailedDecoder =
    Decode.map (\msg -> InitializeFailedResponse { message = msg })
        (Decode.field "message" Decode.string)


authenticateResponseDecoder : Decoder IncomingMsg
authenticateResponseDecoder =
    Decode.map (\token -> AuthenticateResponse { token = token })
        (Decode.field "token" Decode.string)


authenticateFailedDecoder : Decoder IncomingMsg
authenticateFailedDecoder =
    Decode.map (\msg -> AuthenticateFailedResponse { message = msg })
        (Decode.field "message" Decode.string)



-- API


sendGapiRequest : String -> GapiRequestConfig Msg -> Cmd ApiMsg
sendGapiRequest token request =
    Http.request
        { method = request.method
        , headers =
            [ Http.header "Authorization" ("Bearer " ++ token)
            ]
        , url = request.url
        , body = request.body
        , expect = Http.expectString ReceiveGapiResponse
        , timeout = Nothing
        , tracker = Nothing
        }
