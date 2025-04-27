port module Api.Google exposing
    ( Model
    , Msg(..)
    , init
    , subscriptions
    , update
    )

import Api.Google.Migration as Migration
import Api.Google.Requests as Requests
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import OutMsg exposing (OutMsg(..))
import Task



-- PORTS


port messageSenderPort : Encode.Value -> Cmd msg


messageSender : OutgoingPortMsg -> Cmd msg
messageSender msg =
    msg |> portMsgEncoder |> messageSenderPort


port messageReceiverPort : (Decode.Value -> msg) -> Sub msg


messageReceiver : Sub (Msg rootMsg)
messageReceiver =
    messageReceiverPort
        (\value ->
            GotIncomingPortMsg (Decode.decodeValue portMsgDecoder value)
        )



-- MODEL


type alias Model rootMsg =
    { state : State
    , requestQueue : List (Requests.RequestConfig rootMsg)
    , initializeMsg : Maybe rootMsg
    , token : String
    , appFolderId : String
    , mainSheetId : String
    }


type State
    = Uninitialized
    | Initializing
    | Authenticating
    | SettingUpAppFolder
    | Migrating Migration.Model
    | Ready


init : () -> ( Model rootMsg, Cmd (Msg rootMsg) )
init _ =
    ( { state = Uninitialized
      , requestQueue = []
      , initializeMsg = Nothing
      , token = ""
      , appFolderId = ""
      , mainSheetId = ""
      }
    , Cmd.none
    )



-- UPDATE


type Msg rootMsg
    = GotIncomingPortMsg (Result Decode.Error IncomingMsg)
    | SentInitializeRequest rootMsg
    | SentRequest (Requests.RequestConfig rootMsg)
    | GotResponse (Result Http.Error String)
    | GotFindAppFolderResponse (Result Http.Error Requests.DriveResponseFileList)
    | GotCreateAppFolderResponse (Result Http.Error Requests.DriveResponseFileCreate)
    | GotFindMainSheetResponse (Result Http.Error Requests.DriveResponseFileList)
    | GotCreateMainSheetResponse (Result Http.Error Requests.DriveResponseFileCreate)
    | GotMigrationMsg Migration.Msg


update :
    Msg rootMsg
    -> Model rootMsg
    -> ( Model rootMsg, Cmd (Msg rootMsg), OutMsg rootMsg )
update msg model =
    case ( model.state, msg ) of
        ( Uninitialized, SentInitializeRequest rootMsg ) ->
            ( { model | state = Initializing, initializeMsg = Just rootMsg }
            , messageSender InitializeRequest
            , None
            )

        ( Uninitialized, SentRequest request ) ->
            ( { model
                | state = Initializing
                , requestQueue = model.requestQueue ++ [ request ]
              }
            , messageSender InitializeRequest
            , None
            )

        ( SettingUpAppFolder, GotFindAppFolderResponse (Ok response) ) ->
            case response.files of
                file :: _ ->
                    ( { model | appFolderId = file.id }
                    , Task.attempt GotFindMainSheetResponse <|
                        Requests.findMainSheetRequest
                            model.token
                            file.id
                    , None
                    )

                [] ->
                    ( model
                    , Task.attempt GotCreateAppFolderResponse <|
                        Requests.createAppFolderRequest
                            model.token
                    , None
                    )

        ( SettingUpAppFolder, GotFindAppFolderResponse (Err _) ) ->
            ( { model | state = Uninitialized }, Cmd.none, None )

        ( SettingUpAppFolder, GotCreateAppFolderResponse (Ok response) ) ->
            ( { model | appFolderId = response.id }
            , Task.attempt GotCreateMainSheetResponse <|
                Requests.createMainSheetRequest
                    model.token
                    response.id
            , None
            )

        ( SettingUpAppFolder, GotCreateAppFolderResponse (Err _) ) ->
            ( { model | state = Uninitialized }, Cmd.none, None )

        ( SettingUpAppFolder, GotFindMainSheetResponse (Ok response) ) ->
            case response.files of
                file :: _ ->
                    transitionToMigrating { model | mainSheetId = file.id }

                [] ->
                    ( model
                    , Task.attempt GotCreateMainSheetResponse <|
                        Requests.createMainSheetRequest
                            model.token
                            model.appFolderId
                    , None
                    )

        ( SettingUpAppFolder, GotFindMainSheetResponse (Err _) ) ->
            ( { model | state = Uninitialized }, Cmd.none, None )

        ( SettingUpAppFolder, GotCreateMainSheetResponse (Ok response) ) ->
            transitionToMigrating { model | mainSheetId = response.id }

        ( SettingUpAppFolder, GotCreateMainSheetResponse (Err _) ) ->
            ( { model | state = Uninitialized }, Cmd.none, None )

        ( Migrating migrationModel, GotMigrationMsg migrationMsg ) ->
            let
                ( newMigrationModel, migrationCmd, migrationOutMsg ) =
                    Migration.update migrationMsg migrationModel
            in
            case migrationOutMsg of
                Migration.None ->
                    ( { model | state = Migrating newMigrationModel }
                    , Cmd.map GotMigrationMsg migrationCmd
                    , None
                    )

                Migration.Fail _ ->
                    ( { model | state = Uninitialized }, Cmd.none, None )

                Migration.Done ->
                    transitionToReady model

        ( Ready, SentRequest request ) ->
            ( { model | requestQueue = model.requestQueue ++ [ request ] }
            , if List.isEmpty model.requestQueue then
                sendRequest model.token request

              else
                Cmd.none
            , None
            )

        ( Ready, GotResponse response ) ->
            case model.requestQueue of
                request :: [] ->
                    ( { model | requestQueue = [] }
                    , Cmd.none
                    , Single (Requests.decodeExpect request.expect response)
                    )

                request :: rest ->
                    ( { model | requestQueue = rest }
                    , sendRequest model.token request
                    , Single (Requests.decodeExpect request.expect response)
                    )

                [] ->
                    ( { model | state = Ready }, Cmd.none, None )

        ( _, SentRequest request ) ->
            ( { model | requestQueue = model.requestQueue ++ [ request ] }
            , Cmd.none
            , None
            )

        ( _, GotResponse response ) ->
            case model.requestQueue of
                request :: rest ->
                    ( { model | requestQueue = rest }
                    , Cmd.none
                    , Single (Requests.decodeExpect request.expect response)
                    )

                [] ->
                    ( { model | state = Ready }, Cmd.none, None )

        ( _, GotIncomingPortMsg portMsg ) ->
            case portMsg of
                Ok decodedPortMsg ->
                    handleIncomingPortMsg decodedPortMsg model

                Err err ->
                    let
                        _ =
                            Debug.log
                                "error decoding port msg"
                                (Decode.errorToString err)
                    in
                    ( model
                    , Cmd.none
                    , None
                    )

        ( _, _ ) ->
            ( model, Cmd.none, None )


handleIncomingPortMsg :
    IncomingMsg
    -> Model rootMsg
    -> ( Model rootMsg, Cmd (Msg rootMsg), OutMsg rootMsg )
handleIncomingPortMsg msg model =
    case ( model.state, msg ) of
        ( Initializing, InitializedResponse ) ->
            ( { model | state = Authenticating }
            , messageSender AuthenticateRequest
            , None
            )

        ( Initializing, InitializeFailedResponse err ) ->
            let
                _ =
                    Debug.log "initialization error" err
            in
            ( { model | state = Uninitialized }
            , Cmd.none
            , None
            )

        ( Authenticating, AuthenticateResponse res ) ->
            ( { model
                | token = res.token
                , state = SettingUpAppFolder
              }
            , Task.attempt GotFindAppFolderResponse <|
                Requests.findAppFoldersRequest res.token
            , None
            )

        ( Authenticating, AuthenticateFailedResponse err ) ->
            let
                _ =
                    Debug.log "authentication error" err
            in
            ( { model | state = Uninitialized }
            , Cmd.none
            , None
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
            , None
            )


transitionToMigrating :
    Model rootMsg
    -> ( Model rootMsg, Cmd (Msg rootMsg), OutMsg rootMsg )
transitionToMigrating model =
    let
        ( migrationModel, migrationCmd ) =
            Migration.init model.token model.mainSheetId
    in
    ( { model | state = Migrating migrationModel }
    , Cmd.map GotMigrationMsg migrationCmd
    , None
    )


transitionToReady :
    Model rootMsg
    -> ( Model rootMsg, Cmd (Msg rootMsg), OutMsg rootMsg )
transitionToReady model =
    case model.requestQueue of
        request :: _ ->
            ( { model | state = Ready }
            , sendRequest model.token request
            , model.initializeMsg
                |> Maybe.map Single
                |> Maybe.withDefault None
            )

        [] ->
            ( { model | state = Ready }
            , Cmd.none
            , model.initializeMsg
                |> Maybe.map Single
                |> Maybe.withDefault None
            )


sendRequest : String -> Requests.RequestConfig rootMsg -> Cmd (Msg rootMsg)
sendRequest token request =
    Requests.httpRequest
        { token = token
        , method = request.method
        , url = request.url
        , body = request.body
        , expect = Http.expectString GotResponse
        }



-- SUBSCRIPTIONS


subscriptions : Model rootMsg -> Sub (Msg rootMsg)
subscriptions _ =
    messageReceiver



-- OUTGOING PORT MESSAGE


type OutgoingPortMsg
    = InitializeRequest
    | AuthenticateRequest


portMsgEncoder : OutgoingPortMsg -> Encode.Value
portMsgEncoder msg =
    case msg of
        InitializeRequest ->
            Encode.object [ ( "tag", Encode.string "initializeRequest" ) ]

        AuthenticateRequest ->
            Encode.object [ ( "tag", Encode.string "authenticateRequest" ) ]



-- INCOMING PORT MESSAGE


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
