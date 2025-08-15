module Api.Google.Migration exposing
    ( Model
    , Msg
    , OutMsg(..)
    , init
    , update
    )

import Api.Google.Constants as Constants exposing (SubSheet(..))
import Api.Google.Exchange.Sheets as Sheets
    exposing
        ( RequestBatchUpdateKind(..)
        , RequestExtendedValue(..)
        , timestampExtendedValue
        )
import Api.Google.Exchange.SheetsCmd as SheetsCmd
import Api.Google.Exchange.Task as Task
import Api.Google.Migration.Config exposing (Config)
import Api.Google.Migration.Effect as Effect exposing (Effect, EffectInner(..))
import Api.Google.Migration.M00000000SetupMigrations as SetupMigrations
import Api.Google.Migration.M15052025SentencesAndWords as SentencesAndWords
import Set
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
    -> String
    -> Config model msg
    -> Migration
parseConfig toKind toMsg sheetId config =
    { id = config.id
    , kind = toKind config.model
    , initialCmd =
        SheetsCmd.unwrap sheetId config.initialSheetsCmd
            |> Cmd.map toMsg
    }


init : String -> ( Model, Cmd Msg )
init sheetId =
    let
        setupMigrations =
            SetupMigrations.init
                |> parseConfig Setup GotSetupMigrationsMsg sheetId
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
    | MigrationAppended (Result Task.Error ())


type OutMsg
    = None
    | Update String
    | Fail Task.Error
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
                Effect.SheetsTask cmd ->
                    handleRequestBranch
                        Setup
                        GotSetupMigrationsMsg
                        model
                        migration
                        rest
                        ( newSubModel, SheetsCmd.unwrap model.sheetId cmd )

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
            Effect.SheetsTask cmd ->
                handleRequestBranch
                    (toModel >> Normal)
                    (toMsg >> GotMigrationMsg)
                    model
                    migration
                    restMigrations
                    ( newSubModel, SheetsCmd.unwrap model.sheetId cmd )

            Effect.Fail err ->
                handleFailBranch err model

            Effect.Done _ ->
                ( { model | migrationQueue = restMigrations }
                , Task.platform Time.now
                    |> Task.andThen
                        (fillMigrationsRequest migration.id)
                    |> Task.sheetsAttempt MigrationAppended
                    |> SheetsCmd.unwrap model.sheetId
                , None
                )


fillMigrationsRequest :
    String
    -> Time.Posix
    -> Task.SheetsTask ()
fillMigrationsRequest migrationId time =
    Sheets.batchUpdateRequest
        [ RequestAppendCells
            { sheetId = Constants.subSheetId Migrations
            , rows =
                Sheets.requestRow
                    [ RequestString migrationId
                    , timestampExtendedValue time
                    ]
            , fields = "userEnteredValue"
            }
        ]


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


handleFailBranch : Task.Error -> Model -> ( Model, Cmd Msg, OutMsg )
handleFailBranch err model =
    ( { model | migrationQueue = [] }, Cmd.none, Fail err )


entry :
    Config model msg
    -> (msg -> MigrationMsg)
    -> (model -> MigrationModel)
    -> (String -> Migration)
entry config toMsg toModel =
    \sheetId ->
        parseConfig
            (toModel >> Normal)
            (toMsg >> GotMigrationMsg)
            sheetId
            config
