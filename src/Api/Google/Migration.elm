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
import Api.Google.Requests as Requests exposing (SheetRequestBatchUpdateKind(..), SheetRequestExtendedValue(..))
import Api.Google.TaskCmd as TaskCmd
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


queue : List (String -> Migration)
queue =
    [ entry SentencesAndWords.init GotSentencesAndWordsMsg SentencesAndWords ]



-- MODEL


type alias Model =
    { sheetId : String
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


init : String -> ( Model, Cmd Msg )
init sheetId =
    let
        setupMigrations =
            SetupMigrations.init sheetId
                |> parseConfig Setup GotSetupMigrationsMsg
    in
    ( { sheetId = sheetId
      , migrationQueue =
            setupMigrations :: List.map (\constr -> constr sheetId) queue
      }
    , setupMigrations.initialCmd
    )



-- UPDATE


type Msg
    = GotSetupMigrationsMsg SetupMigrations.Msg
    | GotMigrationMsg MigrationMsg
    | MigrationAppended (Result Requests.Error ())


type OutMsg
    = None
    | Update String
    | Fail Requests.Error
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
                            , Update nextMigration.id
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
                    , Update nextMigration.id
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
                            model.sheetId
                            migration.id
                        )
                    |> Task.attempt MigrationAppended
                , None
                )


fillMigrationsRequest :
    String
    -> String
    -> Time.Posix
    -> Task Requests.Error ()
fillMigrationsRequest sheetId migrationId time =
    Requests.sheetBatchUpdateRequest
        [ AppendCells
            { sheetId = Constants.subSheetId Migrations
            , rows =
                Requests.sheetRequestRow
                    [ StringValue migrationId
                    , StringValue <| Iso8601.fromTime time
                    ]
            , fields = "userEnteredValue"
            }
        ]
        sheetId
        |> Requests.buildTask


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


handleFailBranch : Requests.Error -> Model -> ( Model, Cmd Msg, OutMsg )
handleFailBranch err model =
    ( { model | migrationQueue = [] }, Cmd.none, Fail err )


entry :
    (String -> Config model msg)
    -> (msg -> MigrationMsg)
    -> (model -> MigrationModel)
    -> (String -> Migration)
entry initFn toMsg toModel =
    \sheetId ->
        initFn sheetId
            |> parseConfig (toModel >> Normal) (toMsg >> GotMigrationMsg)
