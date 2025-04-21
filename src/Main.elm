port module Main exposing (..)

import Api exposing (Action(..), GapiRequestConfig, gapiGetAppFolderId)
import Browser
import Html exposing (Html)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Page.Mining as Mining
import Triple



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
    | SettingUpAppFolder
    | Authenticated



-- UPDATE


type Msg
    = GotApiMsg ApiMsg
    | GotMiningMsg Mining.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( GotApiMsg subMsg, _ ) ->
            updateApi subMsg model.apiModel
                |> Triple.mapFirst
                    (\updatedModel -> { model | apiModel = updatedModel })
                |> (\( updatedModel, apiCmd, maybeOutMsg ) ->
                        maybeOutMsg
                            |> Maybe.map
                                (\outMsg ->
                                    update outMsg updatedModel
                                )
                            |> Maybe.withDefault ( updatedModel, Cmd.none )
                            |> Tuple.mapSecond
                                (\updateCmd ->
                                    Cmd.batch
                                        [ Cmd.map GotApiMsg apiCmd
                                        , updateCmd
                                        ]
                                )
                   )

        ( GotMiningMsg subMsg, Mining subModel ) ->
            Mining.update subMsg subModel
                |> Triple.mapFirst
                    (\updatedSubModel -> { model | page = Mining updatedSubModel })
                |> (\( updatedSubModel, cmd, action ) ->
                        Api.mapAction GotMiningMsg action
                            |> performApiAction updatedSubModel
                            |> Tuple.mapSecond
                                (\apiActionCmd ->
                                    Cmd.batch
                                        [ Cmd.map GotMiningMsg cmd
                                        , apiActionCmd
                                        ]
                                )
                   )



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

        ( Authenticated, SendGapiRequest request ) ->
            ( { model | requestQueue = model.requestQueue ++ [ request ] }
            , if List.isEmpty model.requestQueue then
                sendGapiRequest model.token request

              else
                Cmd.none
            , Nothing
            )

        ( SettingUpAppFolder, SendGapiRequest request ) ->
            ( model, Cmd.none, Nothing )

        ( Authenticated, ReceiveGapiResponse response ) ->
            case model.requestQueue of
                request :: [] ->
                    ( { model
                        | state = Authenticated
                        , requestQueue = []
                      }
                    , Cmd.none
                    , Just (Api.decodeGapiExpect request.expect response)
                    )

                request :: rest ->
                    ( { model
                        | state = Authenticated
                        , requestQueue = rest
                      }
                    , sendGapiRequest model.token request
                    , Just (Api.decodeGapiExpect request.expect response)
                    )

                [] ->
                    ( { model | state = Authenticated }, Cmd.none, Nothing )

        ( _, SendGapiRequest request ) ->
            ( { model | requestQueue = model.requestQueue ++ [ request ] }
            , Cmd.none
            , Nothing
            )

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
                    ( { model | token = res.token, state = Authenticated }
                    , sendGapiRequest res.token request
                    , Nothing
                    )

                [] ->
                    ( { model | token = res.token, state = Authenticated }
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
        Gapi request ->
            update (GotApiMsg (SendGapiRequest request)) model

        None ->
            ( model, Cmd.none )



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
