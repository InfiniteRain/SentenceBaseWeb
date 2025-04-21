port module Main exposing (..)

import Browser
import Gapi exposing (Action(..))
import Html exposing (Html)
import Http exposing (request)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Page.Mining as Mining exposing (Msg(..))
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


messageReceiver : Sub GapiMsg
messageReceiver =
    messageReceiverPort (\value -> IncomingPortMsg (Decode.decodeValue portMsgDecoder value))



-- MODEL


type alias Model =
    { gapiModel : GapiModel
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
    ( { gapiModel =
            { state = Uninitialized
            , requestQueue = []
            , token = ""
            , appFolderId = ""
            }
      , page = Mining miningModel
      }
    , Cmd.map GotMiningMsg miningCmd
    )



-- GAPI MODEL


type alias GapiModel =
    { state : GapiState
    , requestQueue : List (Gapi.GapiRequestConfig Msg)
    , token : String
    , appFolderId : String
    }


type GapiState
    = Uninitialized
    | Initializing
    | Authenticating
    | SettingUpAppFolder
    | Ready



-- UPDATE


type Msg
    = GotGapiMsg GapiMsg
    | GotMiningMsg Mining.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( GotGapiMsg subMsg, _ ) ->
            gapiUpdate subMsg model.gapiModel
                |> Triple.mapFirst
                    (\updatedModel -> { model | gapiModel = updatedModel })
                |> (\( updatedModel, gapiCmd, maybeOutMsg ) ->
                        maybeOutMsg
                            |> Maybe.map
                                (\outMsg ->
                                    update outMsg updatedModel
                                )
                            |> Maybe.withDefault ( updatedModel, Cmd.none )
                            |> Tuple.mapSecond
                                (\updateCmd ->
                                    Cmd.batch
                                        [ updateCmd
                                        , Cmd.map GotGapiMsg gapiCmd
                                        ]
                                )
                   )

        ( GotMiningMsg subMsg, Mining subModel ) ->
            Mining.update subMsg subModel
                |> Triple.mapFirst
                    (\updatedSubModel -> { model | page = Mining updatedSubModel })
                |> (\( updatedSubModel, cmd, action ) ->
                        Gapi.mapAction GotMiningMsg action
                            |> performGapiAction updatedSubModel
                            |> Tuple.mapSecond
                                (\gapiActionCmd ->
                                    Cmd.batch
                                        [ gapiActionCmd
                                        , Cmd.map GotMiningMsg cmd
                                        ]
                                )
                   )



-- GAPI UPDATE


type GapiMsg
    = IncomingPortMsg (Result Decode.Error IncomingMsg)
    | SendGapiRequest (Gapi.GapiRequestConfig Msg)
    | ReceivedGapiResponse (Result Http.Error String)
    | ReceivedInternalGapiResponse InternalGapiResponse


type InternalGapiResponse
    = FindAppFolders (Result Http.Error Gapi.GapiFileListResponse)
    | CreateAppFolder (Result Http.Error Gapi.GapiFileCreateResponse)


gapiUpdate : GapiMsg -> GapiModel -> ( GapiModel, Cmd GapiMsg, Maybe Msg )
gapiUpdate msg model =
    case ( model.state, msg ) of
        ( Uninitialized, SendGapiRequest request ) ->
            ( { model
                | state = Initializing
                , requestQueue = model.requestQueue ++ [ request ]
              }
            , messageSender InitializeRequest
            , Nothing
            )

        ( SettingUpAppFolder, ReceivedInternalGapiResponse internalResponse ) ->
            case internalResponse of
                FindAppFolders result ->
                    case result of
                        Ok response ->
                            case response.files of
                                file :: _ ->
                                    transitionToReady { model | appFolderId = file.id }

                                [] ->
                                    ( model
                                    , Gapi.gapiCreateAppFolder
                                        model.token
                                        (\r -> ReceivedInternalGapiResponse (CreateAppFolder r))
                                    , Nothing
                                    )

                        Err _ ->
                            ( { model | state = Uninitialized }, Cmd.none, Nothing )

                CreateAppFolder result ->
                    case result of
                        Ok response ->
                            transitionToReady { model | appFolderId = response.id }

                        Err _ ->
                            ( { model | state = Uninitialized }, Cmd.none, Nothing )

        ( Ready, SendGapiRequest request ) ->
            ( { model | requestQueue = model.requestQueue ++ [ request ] }
            , if List.isEmpty model.requestQueue then
                sendGapiRequest model.token request

              else
                Cmd.none
            , Nothing
            )

        ( Ready, ReceivedGapiResponse response ) ->
            case model.requestQueue of
                request :: [] ->
                    ( { model | requestQueue = [] }
                    , Cmd.none
                    , Just (Gapi.decodeGapiExpect request.expect response)
                    )

                request :: rest ->
                    ( { model | requestQueue = rest }
                    , sendGapiRequest model.token request
                    , Just (Gapi.decodeGapiExpect request.expect response)
                    )

                [] ->
                    ( { model | state = Ready }, Cmd.none, Nothing )

        ( _, SendGapiRequest request ) ->
            ( { model | requestQueue = model.requestQueue ++ [ request ] }
            , Cmd.none
            , Nothing
            )

        ( _, ReceivedGapiResponse response ) ->
            case model.requestQueue of
                request :: rest ->
                    ( { model | requestQueue = rest }
                    , Cmd.none
                    , Just (Gapi.decodeGapiExpect request.expect response)
                    )

                [] ->
                    ( { model | state = Ready }, Cmd.none, Nothing )

        ( _, IncomingPortMsg portMsg ) ->
            case portMsg of
                Ok decodedPortMsg ->
                    handleIncomingPortMsg decodedPortMsg model

                Err err ->
                    let
                        _ =
                            Debug.log "error decoding port msg" (Decode.errorToString err)
                    in
                    ( model
                    , Cmd.none
                    , Nothing
                    )

        ( _, _ ) ->
            ( model, Cmd.none, Nothing )


transitionToReady : GapiModel -> ( GapiModel, Cmd GapiMsg, Maybe Msg )
transitionToReady model =
    case model.requestQueue of
        request :: _ ->
            ( { model | state = Ready }
            , sendGapiRequest model.token request
            , Nothing
            )

        [] ->
            ( { model | state = Ready }
            , Cmd.none
            , Nothing
            )


handleIncomingPortMsg : IncomingMsg -> GapiModel -> ( GapiModel, Cmd GapiMsg, Maybe Msg )
handleIncomingPortMsg msg model =
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
            ( { model
                | token = res.token
                , state = SettingUpAppFolder
              }
            , Gapi.gapiFindAppFolders res.token
                (\result ->
                    ReceivedInternalGapiResponse (FindAppFolders result)
                )
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
                    Debug.log
                        "unhandled state/msg combination in handleIncomingMsg"
                        unhandledCombination
            in
            ( model
            , Cmd.none
            , Nothing
            )


performGapiAction : Model -> Gapi.Action Msg -> ( Model, Cmd Msg )
performGapiAction model action =
    case action of
        Gapi request ->
            update (GotGapiMsg (SendGapiRequest request)) model

        None ->
            ( model, Cmd.none )


sendGapiRequest : String -> Gapi.GapiRequestConfig Msg -> Cmd GapiMsg
sendGapiRequest token request =
    Http.request
        { method = request.method
        , headers =
            [ Http.header "Authorization" ("Bearer " ++ token)
            ]
        , url = request.url
        , body = request.body
        , expect = Http.expectString ReceivedGapiResponse
        , timeout = Nothing
        , tracker = Nothing
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.map GotGapiMsg messageReceiver



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
