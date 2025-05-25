module Api.Google.Migration exposing
    ( Model
    , Msg
    , OutMsg(..)
    , init
    , update
    )

import Api.Google.Constants as Constants exposing (SubSheet(..))
import Api.Google.Migration.Config exposing (Config)
import Api.Google.Migration.Effect as Effect exposing (Effect, EffectInner(..))
import Api.Google.Migration.M00000000SetupMigrations as SetupMigrations
import Api.Google.Migration.M15052025SentencesAndWords as SentencesAndWords
import Api.Google.Requests as Requests
import Api.Google.TaskCmd as TaskCmd
import Http
import Iso8601
import Set
import Task exposing (Task)
import Time



-- MIGRATIONS SETUP


type MigrationMsg
    = GotSentencesAndWordsMsg SentencesAndWords.Msg


type MigrationModel
    = SentencesAndWords SentencesAndWords.Model


updateMigration : MigrationMsg -> MigrationModel -> MigrationUpdate
updateMigration msg model =
    case ( msg, model ) of
        ( GotSentencesAndWordsMsg subMsg, SentencesAndWords subModel ) ->
            updateWith
                subMsg
                subModel
                SentencesAndWords.update
                GotSentencesAndWordsMsg
                SentencesAndWords


queue : List (String -> String -> Migration)
queue =
    [ entry SentencesAndWords.init GotSentencesAndWordsMsg SentencesAndWords ]



-- MODEL


type alias Model =
    { token : String
    , sheetId : String
    , migrationQueue : List Migration
    }


type alias Migration =
    { id : String
    , kind : Kind
    , initialCmd : Cmd Msg
    }


type Kind
    = Setup SetupMigrations.Model
    | Normal MigrationModel


parseConfig :
    (model -> Kind)
    -> (msg -> Msg)
    -> Config model msg
    -> Migration
parseConfig toKind toMsg config =
    { id = config.id
    , kind = toKind config.model
    , initialCmd = Cmd.map toMsg <| TaskCmd.cmd config.initialTask
    }


init : String -> String -> ( Model, Cmd Msg )
init token sheetId =
    let
        setupMigrations =
            SetupMigrations.init token sheetId
                |> parseConfig Setup GotSetupMigrationsMsg
    in
    ( { token = token
      , sheetId = sheetId
      , migrationQueue =
            setupMigrations :: List.map (\constr -> constr token sheetId) queue
      }
    , setupMigrations.initialCmd
    )



-- UPDATE


type Msg
    = GotSetupMigrationsMsg SetupMigrations.Msg
    | GotMigrationMsg MigrationMsg
    | MigrationAppended (Result Http.Error ())


type OutMsg
    = None
    | Fail Http.Error
    | Done


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    let
        data =
            case model.migrationQueue of
                ({ kind } as migration) :: rest ->
                    Just ( kind, migration, rest )

                [] ->
                    Nothing
    in
    case ( msg, data ) of
        ( GotSetupMigrationsMsg subMsg, Just ( Setup subModel, migration, rest ) ) ->
            let
                ( newSubModel, effect ) =
                    SetupMigrations.update subMsg subModel
            in
            case effect of
                Effect.Task cmd ->
                    handleRequestBranch
                        Setup
                        GotSetupMigrationsMsg
                        model
                        migration
                        rest
                        ( newSubModel, TaskCmd.cmd cmd )

                Effect.Fail err ->
                    handleFailBranch err model

                Effect.Done appliedMigrations ->
                    let
                        newQueue =
                            List.filter
                                (\appliedMigration ->
                                    not <|
                                        Set.member
                                            appliedMigration.id
                                            appliedMigrations
                                )
                                rest
                    in
                    case newQueue of
                        nextMigration :: _ ->
                            ( { model | migrationQueue = newQueue }
                            , nextMigration.initialCmd
                            , None
                            )

                        [] ->
                            ( { model | migrationQueue = [] }
                            , Cmd.none
                            , Done
                            )

        ( GotMigrationMsg subMsg, Just ( Normal subModel, migration, rest ) ) ->
            updateMigration subMsg subModel model migration rest

        ( MigrationAppended (Err err), _ ) ->
            ( { model | migrationQueue = [] }, Cmd.none, Fail err )

        ( MigrationAppended (Ok _), _ ) ->
            case model.migrationQueue of
                nextMigration :: _ ->
                    ( model
                    , nextMigration.initialCmd
                    , None
                    )

                [] ->
                    ( { model | migrationQueue = [] }, Cmd.none, Done )

        ( _, _ ) ->
            ( model, Cmd.none, None )


type alias MigrationUpdate =
    Model
    -> Migration
    -> List Migration
    -> ( Model, Cmd Msg, OutMsg )


updateWith :
    msg
    -> model
    -> (msg -> model -> ( model, Effect msg ))
    -> (msg -> MigrationMsg)
    -> (model -> MigrationModel)
    -> MigrationUpdate
updateWith subMsg subModel updateFn toMsg toModel =
    let
        ( newSubModel, effect ) =
            updateFn subMsg subModel
    in
    \model migration restMigrations ->
        case effect of
            Effect.Task task ->
                handleRequestBranch
                    (toModel >> Normal)
                    (toMsg >> GotMigrationMsg)
                    model
                    migration
                    restMigrations
                    ( newSubModel, TaskCmd.cmd task )

            Effect.Fail err ->
                handleFailBranch err model

            Effect.Done _ ->
                ( { model | migrationQueue = restMigrations }
                , Time.now
                    |> Task.andThen
                        (fillMigrationsRequest
                            model.token
                            model.sheetId
                            migration.id
                        )
                    |> Task.attempt MigrationAppended
                , None
                )


fillMigrationsRequest :
    String
    -> String
    -> String
    -> Time.Posix
    -> Task Http.Error ()
fillMigrationsRequest token sheetId migrationId date =
    Requests.sheetBatchUpdateRequest
        [ Requests.AppendCells
            { sheetId = Constants.subSheetId Migrations
            , rows =
                [ { values =
                        [ { userEnteredValue =
                                Requests.StringValue migrationId
                          }
                        , { userEnteredValue =
                                Requests.StringValue <|
                                    Iso8601.fromTime date
                          }
                        ]
                  }
                ]
            , fields = "userEnteredValue"
            }
        ]
        token
        sheetId


handleRequestBranch :
    (subModel -> Kind)
    -> (subMsg -> Msg)
    -> Model
    -> Migration
    -> List Migration
    -> ( subModel, Cmd subMsg )
    -> ( Model, Cmd Msg, OutMsg )
handleRequestBranch toKind toMsg model migration restMigrations ( newSubModel, cmd ) =
    ( { model
        | migrationQueue =
            { migration | kind = toKind newSubModel }
                :: restMigrations
      }
    , Cmd.map toMsg cmd
    , None
    )


handleFailBranch : Http.Error -> Model -> ( Model, Cmd Msg, OutMsg )
handleFailBranch err model =
    ( { model | migrationQueue = [] }, Cmd.none, Fail err )


entry :
    (String -> String -> Config model msg)
    -> (msg -> MigrationMsg)
    -> (model -> MigrationModel)
    -> (String -> String -> Migration)
entry initFn toMsg toModel =
    \token sheetId ->
        initFn token sheetId
            |> parseConfig (toModel >> Normal) (toMsg >> GotMigrationMsg)
