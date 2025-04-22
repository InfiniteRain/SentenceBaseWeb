port module Main exposing (..)

import Api.Google as Google exposing (Action(..))
import Browser
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


messageReceiver : Sub GoogleMsg
messageReceiver =
    messageReceiverPort (\value -> IncomingPortMsg (Decode.decodeValue portMsgDecoder value))



-- MODEL


type alias Model =
    { googleModel : GoogleModel
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
    ( { googleModel =
            { state = Uninitialized
            , requestQueue = []
            , token = ""
            , appFolderId = ""
            }
      , page = Mining miningModel
      }
    , Cmd.map GotMiningMsg miningCmd
    )



-- GOOGLE MODEL


type alias GoogleModel =
    { state : GoogleState
    , requestQueue : List (Google.GoogleRequestConfig Msg)
    , token : String
    , appFolderId : String
    }


type GoogleState
    = Uninitialized
    | Initializing
    | Authenticating
    | SettingUpAppFolder
    | Ready



-- UPDATE


type Msg
    = GotGoogleMsg GoogleMsg
    | GotMiningMsg Mining.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( GotGoogleMsg subMsg, _ ) ->
            googleUpdate subMsg model.googleModel
                |> Triple.mapFirst
                    (\updatedModel -> { model | googleModel = updatedModel })
                |> (\( updatedModel, googleCmd, maybeOutMsg ) ->
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
                                        , Cmd.map GotGoogleMsg googleCmd
                                        ]
                                )
                   )

        ( GotMiningMsg subMsg, Mining subModel ) ->
            Mining.update subMsg subModel
                |> Triple.mapFirst
                    (\updatedSubModel -> { model | page = Mining updatedSubModel })
                |> (\( updatedSubModel, cmd, action ) ->
                        Google.mapAction GotMiningMsg action
                            |> performGoogleAction updatedSubModel
                            |> Tuple.mapSecond
                                (\googleActionCmd ->
                                    Cmd.batch
                                        [ googleActionCmd
                                        , Cmd.map GotMiningMsg cmd
                                        ]
                                )
                   )



-- GOOGLE UPDATE


type GoogleMsg
    = IncomingPortMsg (Result Decode.Error IncomingMsg)
    | SendGoogleRequest (Google.GoogleRequestConfig Msg)
    | ReceivedGoogleResponse (Result Http.Error String)
    | ReceivedInternalGoogleResponse InternalGoogleResponse


type InternalGoogleResponse
    = FindAppFolders (Result Http.Error Google.GoogleFileListResponse)
    | CreateAppFolder (Result Http.Error Google.GoogleFileCreateResponse)


googleUpdate : GoogleMsg -> GoogleModel -> ( GoogleModel, Cmd GoogleMsg, Maybe Msg )
googleUpdate msg model =
    case ( model.state, msg ) of
        ( Uninitialized, SendGoogleRequest request ) ->
            ( { model
                | state = Initializing
                , requestQueue = model.requestQueue ++ [ request ]
              }
            , messageSender InitializeRequest
            , Nothing
            )

        ( SettingUpAppFolder, ReceivedInternalGoogleResponse internalResponse ) ->
            case internalResponse of
                FindAppFolders result ->
                    case result of
                        Ok response ->
                            case response.files of
                                file :: _ ->
                                    transitionToReady { model | appFolderId = file.id }

                                [] ->
                                    ( model
                                    , Google.googleCreateAppFolderRequest
                                        model.token
                                        (\r -> ReceivedInternalGoogleResponse (CreateAppFolder r))
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

        ( Ready, SendGoogleRequest request ) ->
            ( { model | requestQueue = model.requestQueue ++ [ request ] }
            , if List.isEmpty model.requestQueue then
                sendGoogleRequest model.token request

              else
                Cmd.none
            , Nothing
            )

        ( Ready, ReceivedGoogleResponse response ) ->
            case model.requestQueue of
                request :: [] ->
                    ( { model | requestQueue = [] }
                    , Cmd.none
                    , Just (Google.decodeGoogleExpect request.expect response)
                    )

                request :: rest ->
                    ( { model | requestQueue = rest }
                    , sendGoogleRequest model.token request
                    , Just (Google.decodeGoogleExpect request.expect response)
                    )

                [] ->
                    ( { model | state = Ready }, Cmd.none, Nothing )

        ( _, SendGoogleRequest request ) ->
            ( { model | requestQueue = model.requestQueue ++ [ request ] }
            , Cmd.none
            , Nothing
            )

        ( _, ReceivedGoogleResponse response ) ->
            case model.requestQueue of
                request :: rest ->
                    ( { model | requestQueue = rest }
                    , Cmd.none
                    , Just (Google.decodeGoogleExpect request.expect response)
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


transitionToReady : GoogleModel -> ( GoogleModel, Cmd GoogleMsg, Maybe Msg )
transitionToReady model =
    case model.requestQueue of
        request :: _ ->
            ( { model | state = Ready }
            , sendGoogleRequest model.token request
            , Nothing
            )

        [] ->
            ( { model | state = Ready }
            , Cmd.none
            , Nothing
            )


handleIncomingPortMsg : IncomingMsg -> GoogleModel -> ( GoogleModel, Cmd GoogleMsg, Maybe Msg )
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
            , Google.googleFindAppFoldersRequest res.token
                (\result ->
                    ReceivedInternalGoogleResponse (FindAppFolders result)
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


performGoogleAction : Model -> Google.Action Msg -> ( Model, Cmd Msg )
performGoogleAction model action =
    case action of
        Google request ->
            update (GotGoogleMsg (SendGoogleRequest request)) model

        None ->
            ( model, Cmd.none )


sendGoogleRequest : String -> Google.GoogleRequestConfig Msg -> Cmd GoogleMsg
sendGoogleRequest token request =
    Http.request
        { method = request.method
        , headers =
            [ Http.header "Authorization" ("Bearer " ++ token)
            ]
        , url = request.url
        , body = request.body
        , expect = Http.expectString ReceivedGoogleResponse
        , timeout = Nothing
        , tracker = Nothing
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map GotGoogleMsg messageReceiver
        , case model.page of
            Mining subModel ->
                Sub.map GotMiningMsg (Mining.subscriptions subModel)
        ]



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
