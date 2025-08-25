module Api.Google exposing
    ( Model
    , Msg(..)
    , init
    , subscriptions
    , update
    )

import Api.Google.Exchange.Drive as Drive
import Api.Google.Exchange.SheetsCmd as DriveCmd exposing (SheetsCmd)
import Api.Google.Exchange.Task as Task
import Api.Google.Migration as Migration
import Api.OutMsg as OutMsg exposing (OutMsg(..))
import Effect
    exposing
        ( Effect
        , GoogleAction(..)
        , InitializeFailure(..)
        , InitializeUpdate(..)
        )
import Json.Decode as Decode
import Json.Encode as Encode
import Task as PlatformTask
import TaskPort



-- MODEL


type alias Model rootMsg =
    { state : State rootMsg
    , requestQueue : List ( Effect rootMsg, SheetsCmd rootMsg )
    , initializeMsg : Maybe (InitializeUpdate -> rootMsg)
    , appFolderId : String
    , mainSheetId : String
    }


type State rootMsg
    = Uninitialized
    | Authenticating (Effect rootMsg)
    | SettingUpAppFolder (Effect rootMsg)
    | Migrating (Effect rootMsg) Migration.Model
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
    | SentAction (Effect rootMsg) (GoogleAction rootMsg)
    | GotActionResponse rootMsg
    | GotFindAppFolderResponse (Result Task.Error Drive.ResponseFileList)
    | GotCreateAppFolderResponse (Result Task.Error Drive.ResponseFileCreate)
    | GotFindMainSheetResponse (Result Task.Error Drive.ResponseFileList)
    | GotCreateMainSheetResponse (Result Task.Error Drive.ResponseFileCreate)
    | GotMigrationMsg Migration.Msg


update :
    Msg rootMsg
    -> Model rootMsg
    -> ( Model rootMsg, Cmd (Msg rootMsg), OutMsg rootMsg )
update msg model =
    case ( model.state, msg ) of
        ( Uninitialized, SentAction effect (Initialize toMsg) ) ->
            ( { model
                | state = Authenticating effect
                , initializeMsg = Just toMsg
              }
            , PlatformTask.attempt GotAuthenticationResult googleGetToken
            , maybeSendInitializationUpdate model effect AuthenticatingApi
            )

        ( Authenticating effect, GotAuthenticationResult (Ok _) ) ->
            ( { model | state = SettingUpAppFolder effect }
            , Drive.findAppFoldersRequest
                |> Task.driveAttempt GotFindAppFolderResponse
            , maybeSendInitializationUpdate model effect LocatingAppFolder
            )

        ( Authenticating effect, GotAuthenticationResult (Err err) ) ->
            ( { model | state = Uninitialized }
            , Cmd.none
            , maybeSendInitializationUpdate
                model
                effect
                (Failed <| ApiAuthentication err)
            )

        ( SettingUpAppFolder effect, GotFindAppFolderResponse (Ok response) ) ->
            case response.files of
                file :: _ ->
                    ( { model | appFolderId = file.id }
                    , Drive.findMainSheetRequest file.id
                        |> Task.driveAttempt GotFindMainSheetResponse
                    , maybeSendInitializationUpdate
                        model
                        effect
                        LocatingMainSheet
                    )

                [] ->
                    ( model
                    , Drive.createAppFolderRequest
                        |> Task.driveAttempt GotCreateAppFolderResponse
                    , maybeSendInitializationUpdate
                        model
                        effect
                        CreatingMainSheet
                    )

        ( SettingUpAppFolder effect, GotFindAppFolderResponse (Err err) ) ->
            ( { model | state = Uninitialized }
            , Cmd.none
            , maybeSendInitializationUpdate
                model
                effect
                (Failed <| AppFolderLocation err)
            )

        ( SettingUpAppFolder effect, GotCreateAppFolderResponse (Ok response) ) ->
            ( { model | appFolderId = response.id }
            , Drive.createMainSheetRequest response.id
                |> Task.driveAttempt GotCreateMainSheetResponse
            , maybeSendInitializationUpdate model effect CreatingMainSheet
            )

        ( SettingUpAppFolder effect, GotCreateAppFolderResponse (Err err) ) ->
            ( { model | state = Uninitialized }
            , Cmd.none
            , maybeSendInitializationUpdate
                model
                effect
                (Failed <| AppFolderCreation err)
            )

        ( SettingUpAppFolder effect, GotFindMainSheetResponse (Ok response) ) ->
            case response.files of
                file :: _ ->
                    transitionToMigrating
                        { model | mainSheetId = file.id }
                        effect

                [] ->
                    ( model
                    , Drive.createMainSheetRequest
                        model.appFolderId
                        |> Task.driveAttempt GotCreateMainSheetResponse
                    , maybeSendInitializationUpdate
                        model
                        effect
                        CreatingMainSheet
                    )

        ( SettingUpAppFolder effect, GotFindMainSheetResponse (Err err) ) ->
            ( { model | state = Uninitialized }
            , Cmd.none
            , maybeSendInitializationUpdate
                model
                effect
                (Failed <| MainSheetLocation err)
            )

        ( SettingUpAppFolder effect, GotCreateMainSheetResponse (Ok response) ) ->
            transitionToMigrating { model | mainSheetId = response.id } effect

        ( SettingUpAppFolder effect, GotCreateMainSheetResponse (Err err) ) ->
            ( { model | state = Uninitialized }
            , Cmd.none
            , maybeSendInitializationUpdate
                model
                effect
                (Failed <| MainSheetCreation err)
            )

        ( Migrating effect migrationModel, GotMigrationMsg migrationMsg ) ->
            let
                ( newMigrationModel, migrationCmd, migrationOutMsg ) =
                    Migration.update migrationMsg migrationModel
            in
            case migrationOutMsg of
                Migration.None ->
                    ( { model | state = Migrating effect newMigrationModel }
                    , Cmd.map GotMigrationMsg migrationCmd
                    , OutMsg.none
                    )

                Migration.Update migrationId ->
                    ( { model | state = Migrating effect newMigrationModel }
                    , Cmd.map GotMigrationMsg migrationCmd
                    , maybeSendInitializationUpdate
                        model
                        effect
                        (MigratingMainSheet migrationId)
                    )

                Migration.Fail err ->
                    ( { model | state = Uninitialized }
                    , Cmd.none
                    , maybeSendInitializationUpdate
                        model
                        effect
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
                    transitionToReady model effect

        ( Ready, SentAction effect (SendRequest sheetsCmd) ) ->
            ( { model
                | requestQueue =
                    model.requestQueue
                        ++ [ ( effect, sheetsCmd ) ]
              }
            , if List.isEmpty model.requestQueue then
                DriveCmd.unwrap model.mainSheetId sheetsCmd
                    |> Cmd.map GotActionResponse

              else
                Cmd.none
            , OutMsg.none
            )

        ( Ready, GotActionResponse response ) ->
            case model.requestQueue of
                ( effect, _ ) :: [] ->
                    ( { model | requestQueue = [] }
                    , Cmd.none
                    , OutMsg.some effect response
                    )

                ( effect, _ ) :: (( _, nextCmd ) as nextPair) :: rest ->
                    ( { model | requestQueue = nextPair :: rest }
                    , DriveCmd.unwrap model.mainSheetId nextCmd
                        |> Cmd.map GotActionResponse
                    , OutMsg.some effect response
                    )

                [] ->
                    ( model, Cmd.none, OutMsg.none )

        ( _, SentAction effect (SendRequest sheetsCmd) ) ->
            ( { model
                | requestQueue =
                    model.requestQueue
                        ++ [ ( effect, sheetsCmd ) ]
              }
            , Cmd.none
            , OutMsg.none
            )

        ( _, GotActionResponse response ) ->
            case model.requestQueue of
                ( effect, _ ) :: rest ->
                    ( { model | requestQueue = rest }
                    , Cmd.none
                    , OutMsg.some effect response
                    )

                [] ->
                    ( { model | state = Ready }, Cmd.none, OutMsg.none )

        ( _, _ ) ->
            ( model, Cmd.none, OutMsg.none )


transitionToMigrating :
    Model rootMsg
    -> Effect rootMsg
    -> ( Model rootMsg, Cmd (Msg rootMsg), OutMsg rootMsg )
transitionToMigrating model effect =
    let
        ( migrationModel, migrationCmd ) =
            Migration.init model.mainSheetId
    in
    ( { model | state = Migrating effect migrationModel }
    , Cmd.map GotMigrationMsg migrationCmd
    , maybeSendInitializationUpdate model effect CheckingMainSheetMigrations
    )


transitionToReady :
    Model rootMsg
    -> Effect rootMsg
    -> ( Model rootMsg, Cmd (Msg rootMsg), OutMsg rootMsg )
transitionToReady model effect =
    case model.requestQueue of
        request :: _ ->
            ( { model | state = Ready }
            , DriveCmd.unwrap model.mainSheetId (Tuple.second request)
                |> Cmd.map GotActionResponse
            , maybeSendInitializationUpdate model effect Done
            )

        [] ->
            ( { model | state = Ready }
            , Cmd.none
            , maybeSendInitializationUpdate model effect Done
            )


maybeSendInitializationUpdate :
    Model rootMsg
    -> Effect rootMsg
    -> InitializeUpdate
    -> OutMsg rootMsg
maybeSendInitializationUpdate model effect initializeUpdate =
    model.initializeMsg
        |> Maybe.map (sendInitializationUpdate effect initializeUpdate)
        |> Maybe.withDefault OutMsg.none


sendInitializationUpdate :
    Effect rootMsg
    -> InitializeUpdate
    -> (InitializeUpdate -> rootMsg)
    -> OutMsg rootMsg
sendInitializationUpdate effect initializeUpdate toMsg =
    OutMsg.some effect (toMsg <| initializeUpdate)



-- SUBSCRIPTIONS


subscriptions : Model rootMsg -> Sub (Msg rootMsg)
subscriptions _ =
    Sub.none



-- PORT


googleGetToken : TaskPort.Task String
googleGetToken =
    TaskPort.call
        { function = "googleGetToken"
        , valueDecoder = Decode.string
        , argsEncoder = \() -> Encode.bool False
        }
        ()
