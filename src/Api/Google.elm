module Api.Google exposing
    ( Action(..)
    , Model
    , Msg(..)
    , init
    , subscriptions
    , update
    )

import Api.Google.Migration as Migration
import Api.Google.ParamCmd exposing (ParamCmd)
import Api.Google.Requests as Requests
import OutMsg exposing (OutMsg(..))
import Port
import Task
import TaskPort



-- MODEL


type alias Model rootMsg =
    { state : State
    , requestQueue : List (ParamCmd rootMsg)
    , initializeMsg : Maybe rootMsg
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
      , appFolderId = ""
      , mainSheetId = ""
      }
    , Cmd.none
    )



-- UPDATE


type Msg rootMsg
    = GotInitializedResult (TaskPort.Result ())
    | GotAuthenticationResult (TaskPort.Result String)
    | SentAction (Action rootMsg)
    | GotActionResponse rootMsg
    | GotFindAppFolderResponse (Result Requests.Error Requests.DriveResponseFileList)
    | GotCreateAppFolderResponse (Result Requests.Error Requests.DriveResponseFileCreate)
    | GotFindMainSheetResponse (Result Requests.Error Requests.DriveResponseFileList)
    | GotCreateMainSheetResponse (Result Requests.Error Requests.DriveResponseFileCreate)
    | GotMigrationMsg Migration.Msg


update :
    Msg rootMsg
    -> Model rootMsg
    -> ( Model rootMsg, Cmd (Msg rootMsg), OutMsg rootMsg )
update msg model =
    case ( model.state, msg ) of
        ( Uninitialized, SentAction (Initialize rootMsg) ) ->
            ( { model | state = Initializing, initializeMsg = Just rootMsg }
            , Task.attempt GotInitializedResult Port.googleInitialize
            , OutMsg.none
            )

        ( Uninitialized, SentAction (SendRequest paramCmd) ) ->
            ( { model
                | state = Initializing
                , requestQueue = model.requestQueue ++ [ paramCmd ]
              }
            , Task.attempt GotInitializedResult Port.googleInitialize
            , OutMsg.none
            )

        ( Initializing, GotInitializedResult (Ok ()) ) ->
            ( { model | state = Authenticating }
            , Task.attempt GotAuthenticationResult Port.googleGetToken
            , OutMsg.none
            )

        ( Initializing, GotInitializedResult (Err err) ) ->
            let
                _ =
                    Debug.log "google initialize error" err
            in
            ( { model | state = Uninitialized }
            , Cmd.none
            , OutMsg.none
            )

        ( Authenticating, GotAuthenticationResult (Ok _) ) ->
            ( { model | state = SettingUpAppFolder }
            , Requests.findAppFoldersRequest
                |> Requests.buildTask
                |> Task.attempt GotFindAppFolderResponse
            , OutMsg.none
            )

        ( Authenticating, GotAuthenticationResult (Err err) ) ->
            let
                _ =
                    Debug.log "google authentication error" err
            in
            ( { model | state = Uninitialized }
            , Cmd.none
            , OutMsg.none
            )

        ( SettingUpAppFolder, GotFindAppFolderResponse (Ok response) ) ->
            case response.files of
                file :: _ ->
                    ( { model | appFolderId = file.id }
                    , Requests.findMainSheetRequest file.id
                        |> Requests.buildTask
                        |> Task.attempt GotFindMainSheetResponse
                    , OutMsg.none
                    )

                [] ->
                    ( model
                    , Requests.createAppFolderRequest
                        |> Requests.buildTask
                        |> Task.attempt GotCreateAppFolderResponse
                    , OutMsg.none
                    )

        ( SettingUpAppFolder, GotFindAppFolderResponse (Err _) ) ->
            ( { model | state = Uninitialized }, Cmd.none, OutMsg.none )

        ( SettingUpAppFolder, GotCreateAppFolderResponse (Ok response) ) ->
            ( { model | appFolderId = response.id }
            , Requests.createMainSheetRequest response.id
                |> Requests.buildTask
                |> Task.attempt GotCreateMainSheetResponse
            , OutMsg.none
            )

        ( SettingUpAppFolder, GotCreateAppFolderResponse (Err _) ) ->
            ( { model | state = Uninitialized }, Cmd.none, OutMsg.none )

        ( SettingUpAppFolder, GotFindMainSheetResponse (Ok response) ) ->
            case response.files of
                file :: _ ->
                    transitionToMigrating { model | mainSheetId = file.id }

                [] ->
                    ( model
                    , Requests.createMainSheetRequest
                        model.appFolderId
                        |> Requests.buildTask
                        |> Task.attempt GotCreateMainSheetResponse
                    , OutMsg.none
                    )

        ( SettingUpAppFolder, GotFindMainSheetResponse (Err _) ) ->
            ( { model | state = Uninitialized }, Cmd.none, OutMsg.none )

        ( SettingUpAppFolder, GotCreateMainSheetResponse (Ok response) ) ->
            transitionToMigrating { model | mainSheetId = response.id }

        ( SettingUpAppFolder, GotCreateMainSheetResponse (Err _) ) ->
            ( { model | state = Uninitialized }, Cmd.none, OutMsg.none )

        ( Migrating migrationModel, GotMigrationMsg migrationMsg ) ->
            let
                ( newMigrationModel, migrationCmd, migrationOutMsg ) =
                    Migration.update migrationMsg migrationModel
            in
            case migrationOutMsg of
                Migration.None ->
                    ( { model | state = Migrating newMigrationModel }
                    , Cmd.map GotMigrationMsg migrationCmd
                    , OutMsg.none
                    )

                Migration.Fail _ ->
                    ( { model | state = Uninitialized }, Cmd.none, OutMsg.none )

                Migration.Done ->
                    transitionToReady model

        ( Ready, SentAction (SendRequest paramCmd) ) ->
            ( { model | requestQueue = model.requestQueue ++ [ paramCmd ] }
            , if List.isEmpty model.requestQueue then
                paramCmd model.mainSheetId
                    |> Cmd.map GotActionResponse

              else
                Cmd.none
            , OutMsg.none
            )

        ( Ready, GotActionResponse response ) ->
            case model.requestQueue of
                _ :: [] ->
                    ( { model | requestQueue = [] }
                    , Cmd.none
                    , OutMsg.some response
                    )

                paramCmd :: rest ->
                    ( { model | requestQueue = rest }
                    , paramCmd model.mainSheetId
                        |> Cmd.map GotActionResponse
                    , OutMsg.some response
                    )

                [] ->
                    ( { model | state = Ready }, Cmd.none, OutMsg.none )

        ( _, SentAction (SendRequest paramCmd) ) ->
            ( { model | requestQueue = model.requestQueue ++ [ paramCmd ] }
            , Cmd.none
            , OutMsg.none
            )

        ( _, GotActionResponse response ) ->
            case model.requestQueue of
                _ :: rest ->
                    ( { model | requestQueue = rest }
                    , Cmd.none
                    , OutMsg.some response
                    )

                [] ->
                    ( { model | state = Ready }, Cmd.none, OutMsg.none )

        ( _, _ ) ->
            ( model, Cmd.none, OutMsg.none )


transitionToMigrating :
    Model rootMsg
    -> ( Model rootMsg, Cmd (Msg rootMsg), OutMsg rootMsg )
transitionToMigrating model =
    let
        ( migrationModel, migrationCmd ) =
            Migration.init model.mainSheetId
    in
    ( { model | state = Migrating migrationModel }
    , Cmd.map GotMigrationMsg migrationCmd
    , OutMsg.none
    )


transitionToReady :
    Model rootMsg
    -> ( Model rootMsg, Cmd (Msg rootMsg), OutMsg rootMsg )
transitionToReady model =
    case model.requestQueue of
        request :: _ ->
            ( { model | state = Ready }
            , request model.mainSheetId
                |> Cmd.map GotActionResponse
            , model.initializeMsg
                |> Maybe.map OutMsg.some
                |> Maybe.withDefault OutMsg.none
            )

        [] ->
            ( { model | state = Ready }
            , Cmd.none
            , model.initializeMsg
                |> Maybe.map OutMsg.some
                |> Maybe.withDefault OutMsg.none
            )



-- SUBSCRIPTIONS


subscriptions : Model rootMsg -> Sub (Msg rootMsg)
subscriptions _ =
    Sub.none



-- EXTERNAL API


type Action msg
    = Initialize msg
    | SendRequest (ParamCmd msg)
