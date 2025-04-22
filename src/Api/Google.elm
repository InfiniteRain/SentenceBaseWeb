port module Api.Google exposing
    ( Error(..)
    , Model
    , Msg(..)
    , RequestConfig
    , getAppFolderId
    , init
    , mapExpect
    , subscriptions
    , update
    )

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import OutMsg exposing (OutMsg(..))
import Url.Builder exposing (QueryParameter, crossOrigin, string)



-- PORTS


port messageSenderPort : Encode.Value -> Cmd msg


messageSender : OutgoingPortMsg -> Cmd msg
messageSender msg =
    msg |> portMsgEncoder |> messageSenderPort


port messageReceiverPort : (Decode.Value -> msg) -> Sub msg


messageReceiver : Sub (Msg rootMsg)
messageReceiver =
    messageReceiverPort (\value -> ReceivedIncomingPortMsg (Decode.decodeValue portMsgDecoder value))



-- MODEL


type alias Model rootMsg =
    { state : State
    , requestQueue : List (RequestConfig rootMsg)
    , token : String
    , appFolderId : String
    }


type State
    = Uninitialized
    | Initializing
    | Authenticating
    | SettingUpAppFolder
    | Ready


init : () -> ( Model rootMsg, Cmd (Msg rootMsg) )
init _ =
    ( { state = Uninitialized
      , requestQueue = []
      , token = ""
      , appFolderId = ""
      }
    , Cmd.none
    )



-- UPDATE


type Msg rootMsg
    = ReceivedIncomingPortMsg (Result Decode.Error IncomingMsg)
    | SentRequest (RequestConfig rootMsg)
    | ReceivedResponse (Result Http.Error String)
    | ReceivedInternalResponse InternalResponse


type InternalResponse
    = FindAppFolders (Result Http.Error FileListResponse)
    | CreateAppFolder (Result Http.Error FileCreateResponse)


update : Msg rootMsg -> Model rootMsg -> ( Model rootMsg, Cmd (Msg rootMsg), OutMsg rootMsg )
update msg model =
    case ( model.state, msg ) of
        ( Uninitialized, SentRequest request ) ->
            ( { model
                | state = Initializing
                , requestQueue = model.requestQueue ++ [ request ]
              }
            , messageSender InitializeRequest
            , None
            )

        ( SettingUpAppFolder, ReceivedInternalResponse internalResponse ) ->
            case internalResponse of
                FindAppFolders result ->
                    case result of
                        Ok response ->
                            case response.files of
                                file :: _ ->
                                    transitionToReady { model | appFolderId = file.id }

                                [] ->
                                    ( model
                                    , createAppFolderRequest
                                        model.token
                                        (\r -> ReceivedInternalResponse (CreateAppFolder r))
                                    , None
                                    )

                        Err _ ->
                            ( { model | state = Uninitialized }, Cmd.none, None )

                CreateAppFolder result ->
                    case result of
                        Ok response ->
                            transitionToReady { model | appFolderId = response.id }

                        Err _ ->
                            ( { model | state = Uninitialized }, Cmd.none, None )

        ( Ready, SentRequest request ) ->
            ( { model | requestQueue = model.requestQueue ++ [ request ] }
            , if List.isEmpty model.requestQueue then
                sendRequest model.token request

              else
                Cmd.none
            , None
            )

        ( Ready, ReceivedResponse response ) ->
            case model.requestQueue of
                request :: [] ->
                    ( { model | requestQueue = [] }
                    , Cmd.none
                    , Single (decodeExpect request.expect response)
                    )

                request :: rest ->
                    ( { model | requestQueue = rest }
                    , sendRequest model.token request
                    , Single (decodeExpect request.expect response)
                    )

                [] ->
                    ( { model | state = Ready }, Cmd.none, None )

        ( _, SentRequest request ) ->
            ( { model | requestQueue = model.requestQueue ++ [ request ] }
            , Cmd.none
            , None
            )

        ( _, ReceivedResponse response ) ->
            case model.requestQueue of
                request :: rest ->
                    ( { model | requestQueue = rest }
                    , Cmd.none
                    , Single (decodeExpect request.expect response)
                    )

                [] ->
                    ( { model | state = Ready }, Cmd.none, None )

        ( _, ReceivedIncomingPortMsg portMsg ) ->
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
                    , None
                    )

        ( _, _ ) ->
            ( model, Cmd.none, None )


handleIncomingPortMsg : IncomingMsg -> Model rootMsg -> ( Model rootMsg, Cmd (Msg rootMsg), OutMsg rootMsg )
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
            , findAppFoldersRequest res.token
                (\result ->
                    ReceivedInternalResponse (FindAppFolders result)
                )
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


transitionToReady : Model rootMsg -> ( Model rootMsg, Cmd (Msg rootMsg), OutMsg rootMsg )
transitionToReady model =
    case model.requestQueue of
        request :: _ ->
            ( { model | state = Ready }
            , sendRequest model.token request
            , None
            )

        [] ->
            ( { model | state = Ready }
            , Cmd.none
            , None
            )


sendRequest : String -> RequestConfig rootMsg -> Cmd (Msg rootMsg)
sendRequest token request =
    httpRequest
        { token = token
        , method = request.method
        , url = request.url
        , body = request.body
        , expect = Http.expectString ReceivedResponse
        }



-- SUBSCRIPTIONS


subscriptions : Model rootMsg -> Sub (Msg rootMsg)
subscriptions _ =
    messageReceiver



-- EXTERNAL API


type Error
    = HttpError Http.Error
    | DecodeError Decode.Error


type Expect msg
    = GetAppFolderResponse (Decoder FileListResponse) (Result Error (Maybe String) -> msg)
    | CreateAppFolderResponse msg


mapExpect : (a -> expect) -> Expect a -> Expect expect
mapExpect map expect =
    case expect of
        GetAppFolderResponse decoder toMsg ->
            GetAppFolderResponse decoder (\result -> map (toMsg result))

        CreateAppFolderResponse msg ->
            CreateAppFolderResponse (map msg)


type alias RequestConfig msg =
    { method : String
    , url : String
    , body : Http.Body
    , expect : Expect msg
    }


decodeExpect : Expect msg -> Result Http.Error String -> msg
decodeExpect expect result =
    let
        mappedResult =
            Result.mapError HttpError result
    in
    case expect of
        GetAppFolderResponse decoder map ->
            decodeMappedResult mappedResult decoder
                |> Result.map
                    (\response ->
                        List.head response.files
                            |> Maybe.map (\file -> file.id)
                    )
                |> map

        CreateAppFolderResponse msg ->
            msg


decodeMappedResult : Result Error String -> Decoder a -> Result Error a
decodeMappedResult mappedResult decoder =
    mappedResult
        |> Result.andThen
            (\str ->
                Decode.decodeString decoder str
                    |> Result.mapError DecodeError
            )


getAppFolderId : (Result Error (Maybe String) -> msg) -> RequestConfig msg
getAppFolderId msg =
    { method = "GET"
    , url =
        googleUrl
            (googleDriveRoute [ "files" ])
            [ string "q"
                ("name = '"
                    ++ appFolderName
                    ++ "' and mimeType = '"
                    ++ googleMimeTypes.folder
                    ++ "'"
                )
            ]
    , body = Http.emptyBody
    , expect = GetAppFolderResponse fileListResponseDecoder msg
    }



-- INTERNAL API


findAppFoldersRequest : String -> (Result Http.Error FileListResponse -> msg) -> Cmd msg
findAppFoldersRequest token msg =
    httpRequest
        { token = token
        , method = "GET"
        , url =
            googleUrl
                (googleDriveRoute [ "files" ])
                [ string "q"
                    ("name = '"
                        ++ appFolderName
                        ++ "' and mimeType = '"
                        ++ googleMimeTypes.folder
                        ++ "'"
                    )
                , string "orderBy" "createdTime"
                ]
        , body = Http.emptyBody
        , expect = Http.expectJson msg fileListResponseDecoder
        }


createAppFolderRequest : String -> (Result Http.Error FileCreateResponse -> msg) -> Cmd msg
createAppFolderRequest token msg =
    httpRequest
        { token = token
        , method = "POST"
        , url = googleUrl (googleDriveRoute [ "files" ]) []
        , body =
            Http.jsonBody
                (fileCreateEncoder
                    { name = appFolderName
                    , mimeType = googleMimeTypes.folder
                    }
                )
        , expect = Http.expectJson msg fileCreateDecoder
        }



-- OUTGOING MESSAGE


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



-- DECODERS


type alias File =
    { kind : String
    , mimeType : String
    , id : String
    , name : String
    }


fileDecoder : Decoder File
fileDecoder =
    Decode.map4 File
        (Decode.field "kind" Decode.string)
        (Decode.field "mimeType" Decode.string)
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)


type alias FileListResponse =
    { files : List File
    }


fileListResponseDecoder : Decoder FileListResponse
fileListResponseDecoder =
    Decode.map FileListResponse
        (Decode.field "files" (Decode.list fileDecoder))


type alias FileCreateResponse =
    { id : String }


fileCreateDecoder : Decoder FileCreateResponse
fileCreateDecoder =
    Decode.map FileCreateResponse
        (Decode.field "id" Decode.string)



-- ENCODERS


type alias FileCreate =
    { name : String
    , mimeType : String
    }


fileCreateEncoder : FileCreate -> Encode.Value
fileCreateEncoder createFile =
    Encode.object
        [ ( "mimeType", Encode.string createFile.mimeType )
        , ( "name", Encode.string createFile.name )
        ]



-- BUILDERS


googleUrl : List String -> List QueryParameter -> String
googleUrl segments params =
    crossOrigin "https://www.googleapis.com" segments params


googleDriveRoute : List String -> List String
googleDriveRoute additional =
    [ "drive", "v3" ] ++ additional


tokenHeader : String -> Http.Header
tokenHeader token =
    Http.header "Authorization" ("Bearer " ++ token)


httpRequest :
    { token : String
    , method : String
    , url : String
    , body : Http.Body
    , expect : Http.Expect msg
    }
    -> Cmd msg
httpRequest { token, method, url, body, expect } =
    Http.request
        { method = method
        , headers = [ tokenHeader token ]
        , url = url
        , body = body
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }



-- REUSED VALUES


appFolderName : String
appFolderName =
    "SentenceBaseData"


googleMimeTypes : { folder : String }
googleMimeTypes =
    { folder = "application/vnd.google-apps.folder"
    }
