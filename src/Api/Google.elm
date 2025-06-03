module Api.Google exposing
    ( Action(..)
    , InitializeFailure(..)
    , InitializeUpdate(..)
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
    , initializeMsg : Maybe (InitializeUpdate -> rootMsg)
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
    | GotFindAppFolderResponse
        (Result
            Requests.Error
            Requests.DriveResponseFileList
        )
    | GotCreateAppFolderResponse
        (Result
            Requests.Error
            Requests.DriveResponseFileCreate
        )
    | GotFindMainSheetResponse
        (Result
            Requests.Error
            Requests.DriveResponseFileList
        )
    | GotCreateMainSheetResponse
        (Result
            Requests.Error
            Requests.DriveResponseFileCreate
        )
    | GotMigrationMsg Migration.Msg


update :
    Msg rootMsg
    -> Model rootMsg
    -> ( Model rootMsg, Cmd (Msg rootMsg), OutMsg rootMsg )
update msg model =
    case ( model.state, msg ) of
        ( Uninitialized, SentAction (Initialize toMsg) ) ->
            ( { model | state = Initializing, initializeMsg = Just toMsg }
            , Task.attempt GotInitializedResult Port.googleInitialize
            , sendInitializationUpdate InitializingApi toMsg
            )

        ( Initializing, GotInitializedResult (Ok ()) ) ->
            ( { model | state = Authenticating }
            , Task.attempt GotAuthenticationResult Port.googleGetToken
            , maybeSendInitializationUpdate model AuthenticatingApi
            )

        ( Initializing, GotInitializedResult (Err err) ) ->
            ( { model | state = Uninitialized }
            , Cmd.none
            , maybeSendInitializationUpdate model
                (Failed <| ApiInitialization err)
            )

        ( Authenticating, GotAuthenticationResult (Ok _) ) ->
            ( { model | state = SettingUpAppFolder }
            , Requests.findAppFoldersRequest
                |> Requests.buildTask
                |> Task.attempt GotFindAppFolderResponse
            , maybeSendInitializationUpdate model LocatingAppFolder
            )

        ( Authenticating, GotAuthenticationResult (Err err) ) ->
            ( { model | state = Uninitialized }
            , Cmd.none
            , maybeSendInitializationUpdate model
                (Failed <| ApiAuthentication err)
            )

        ( SettingUpAppFolder, GotFindAppFolderResponse (Ok response) ) ->
            case response.files of
                file :: _ ->
                    ( { model | appFolderId = file.id }
                    , Requests.findMainSheetRequest file.id
                        |> Requests.buildTask
                        |> Task.attempt GotFindMainSheetResponse
                    , maybeSendInitializationUpdate model LocatingMainSheet
                    )

                [] ->
                    ( model
                    , Requests.createAppFolderRequest
                        |> Requests.buildTask
                        |> Task.attempt GotCreateAppFolderResponse
                    , maybeSendInitializationUpdate model CreatingMainSheet
                    )

        ( SettingUpAppFolder, GotFindAppFolderResponse (Err err) ) ->
            ( { model | state = Uninitialized }
            , Cmd.none
            , maybeSendInitializationUpdate model
                (Failed <| AppFolderLocation err)
            )

        ( SettingUpAppFolder, GotCreateAppFolderResponse (Ok response) ) ->
            ( { model | appFolderId = response.id }
            , Requests.createMainSheetRequest response.id
                |> Requests.buildTask
                |> Task.attempt GotCreateMainSheetResponse
            , maybeSendInitializationUpdate model CreatingMainSheet
            )

        ( SettingUpAppFolder, GotCreateAppFolderResponse (Err err) ) ->
            ( { model | state = Uninitialized }
            , Cmd.none
            , maybeSendInitializationUpdate model
                (Failed <| AppFolderCreation err)
            )

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
                    , maybeSendInitializationUpdate model CreatingMainSheet
                    )

        ( SettingUpAppFolder, GotFindMainSheetResponse (Err err) ) ->
            ( { model | state = Uninitialized }
            , Cmd.none
            , maybeSendInitializationUpdate model
                (Failed <| MainSheetLocation err)
            )

        ( SettingUpAppFolder, GotCreateMainSheetResponse (Ok response) ) ->
            transitionToMigrating { model | mainSheetId = response.id }

        ( SettingUpAppFolder, GotCreateMainSheetResponse (Err err) ) ->
            ( { model | state = Uninitialized }
            , Cmd.none
            , maybeSendInitializationUpdate model
                (Failed <| MainSheetCreation err)
            )

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

                Migration.Update migrationId ->
                    ( { model | state = Migrating newMigrationModel }
                    , Cmd.map GotMigrationMsg migrationCmd
                    , maybeSendInitializationUpdate model
                        (MigratingMainSheet migrationId)
                    )

                Migration.Fail err ->
                    ( { model | state = Uninitialized }
                    , Cmd.none
                    , maybeSendInitializationUpdate model
                        (Failed <|
                            MainSheetMigration
                                (List.head migrationModel.migrationQueue
                                    |> Maybe.map .id
                                    |> Maybe.withDefault "unknown"
                                )
                                err
                        )
                    )

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
    , maybeSendInitializationUpdate model CheckingMainSheetMigrations
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
            , maybeSendInitializationUpdate model Done
            )

        [] ->
            ( { model | state = Ready }
            , Cmd.none
            , maybeSendInitializationUpdate model Done
            )


maybeSendInitializationUpdate :
    Model rootMsg
    -> InitializeUpdate
    -> OutMsg rootMsg
maybeSendInitializationUpdate model initializeUpdate =
    model.initializeMsg
        |> Maybe.map (sendInitializationUpdate initializeUpdate)
        |> Maybe.withDefault OutMsg.none


sendInitializationUpdate :
    InitializeUpdate
    -> (InitializeUpdate -> rootMsg)
    -> OutMsg rootMsg
sendInitializationUpdate initializeUpdate toMsg =
    OutMsg.some <| toMsg <| initializeUpdate



-- SUBSCRIPTIONS


subscriptions : Model rootMsg -> Sub (Msg rootMsg)
subscriptions _ =
    Sub.none



-- EXTERNAL API


type Action msg
    = Initialize (InitializeUpdate -> msg)
    | SendRequest (ParamCmd msg)


type InitializeUpdate
    = InitializingApi
    | AuthenticatingApi
    | LocatingAppFolder
    | CreatingAppFolder
    | LocatingMainSheet
    | CreatingMainSheet
    | CheckingMainSheetMigrations
    | MigratingMainSheet String
    | Done
    | Failed InitializeFailure


type InitializeFailure
    = ApiInitialization TaskPort.Error
    | ApiAuthentication TaskPort.Error
    | AppFolderLocation Requests.Error
    | AppFolderCreation Requests.Error
    | MainSheetLocation Requests.Error
    | MainSheetCreation Requests.Error
    | MainSheetMigration String Requests.Error
