module Api.Google.Migration.M00000000SetupMigrations exposing
    ( Model
    , Msg
    , init
    , update
    )

import Api.Google.Constants as Constants exposing (SubSheet(..))
import Api.Google.Exchange.Sheets as Sheets
    exposing
        ( RequestBatchUpdateKind(..)
        , ResponseCellExtendedData(..)
        )
import Api.Google.Exchange.Task as Task
import Api.Google.ListConstructor exposing (cellStringValue)
import Api.Google.Migration.Config as Config_
import Api.Google.Migration.Effect as Effect exposing (EffectWithPayload)
import Api.Google.Model as Model
import Http
import Set exposing (Set)



-- MODEL


type alias Model =
    { appliedMigrations : Set String
    }


type alias Config =
    Config_.Config Model Msg


init : Config
init =
    { id = "SetupMigrations"
    , model =
        { appliedMigrations = Set.empty
        }
    , initialSheetsCmd =
        Sheets.getSubSheetDataRequest
            [ { sheetId = Constants.subSheetId Migrations
              , startRowIndex = Just 1
              , endRowIndex = Nothing
              , startColumnIndex = Just 0
              , endColumnIndex = Just 1
              }
            ]
            |> Task.sheetsAttempt GotMigrationsSubSheetDataResponse
    }



-- UPDATE


type alias SubSheetDataResult =
    Result
        Task.Error
        Sheets.ResponseGetSubSheetData


type Msg
    = GotMigrationsSubSheetDataResponse SubSheetDataResult
    | GotCreateMigrationsSubSheetResponse (Result Task.Error ())
    | GotQuerySubSheetDataResponse SubSheetDataResult
    | GotCreateQuerySubSheetResponse (Result Task.Error ())
    | GotRenameQuerySubSheetEffect (Result Task.Error ())


update : Msg -> Model -> ( Model, EffectWithPayload Msg (Set String) )
update msg model =
    case msg of
        GotMigrationsSubSheetDataResponse (Err (Task.HttpError (Http.BadStatus 400))) ->
            ( model
            , createSubSheetEffect GotCreateMigrationsSubSheetResponse
                { id = 100
                , name = "migrations"
                , columns =
                    [ ( "name", MigrationName )
                    , ( "applied_at", Timestamp )
                    ]
                }
            )

        GotMigrationsSubSheetDataResponse (Err err) ->
            ( model, Effect.fail err )

        GotMigrationsSubSheetDataResponse (Ok response) ->
            ( { model
                | appliedMigrations = extractAppliedMigrations response
              }
            , getQueryCellEffect
            )

        GotCreateMigrationsSubSheetResponse (Err err) ->
            ( model, Effect.fail err )

        GotCreateMigrationsSubSheetResponse (Ok _) ->
            ( model, getQueryCellEffect )

        GotQuerySubSheetDataResponse (Err (Task.HttpError (Http.BadStatus 400))) ->
            ( model
            , createSubSheetEffect GotCreateQuerySubSheetResponse
                { id = 0
                , name = "query"
                , columns = []
                }
            )

        GotQuerySubSheetDataResponse (Err err) ->
            ( model, Effect.fail err )

        GotQuerySubSheetDataResponse (Ok response) ->
            if extractTitle response == Just querySheetName then
                ( model
                , Effect.doneWithPayload model.appliedMigrations
                )

            else
                ( model
                , Sheets.batchUpdateRequest
                    [ RequestUpdateSheetProperties
                        { properties =
                            { sheetId = Just querySheetId
                            , title = Just querySheetName
                            , gridProperties = Nothing
                            }
                        , fields = "title"
                        }
                    ]
                    |> Effect.sheetsTask GotRenameQuerySubSheetEffect
                )

        GotRenameQuerySubSheetEffect (Err err) ->
            ( model, Effect.fail err )

        GotRenameQuerySubSheetEffect (Ok _) ->
            ( model
            , Effect.doneWithPayload model.appliedMigrations
            )

        GotCreateQuerySubSheetResponse (Err err) ->
            ( model, Effect.fail err )

        GotCreateQuerySubSheetResponse (Ok _) ->
            ( model
            , Effect.doneWithPayload model.appliedMigrations
            )


createSubSheetEffect :
    (Result Task.Error () -> Msg)
    -> Sheets.SubSheet Column
    -> EffectWithPayload Msg (Set String)
createSubSheetEffect toMsg subSheetConfig =
    Sheets.batchUpdateRequest
        (Sheets.addSubSheetRequests columnSize [ subSheetConfig ])
        |> Effect.sheetsTask toMsg


getQueryCellEffect : EffectWithPayload Msg (Set String)
getQueryCellEffect =
    Sheets.getSubSheetDataRequest
        [ { sheetId = Constants.subSheetId Query
          , startRowIndex = Just 0
          , endRowIndex = Just 1
          , startColumnIndex = Just 0
          , endColumnIndex = Just 1
          }
        ]
        |> Effect.sheetsTask GotQuerySubSheetDataResponse


extractAppliedMigrations : Sheets.ResponseGetSubSheetData -> Set String
extractAppliedMigrations =
    Model.fromGridData maybeConstructAppliedMigration
        >> Set.fromList


maybeConstructAppliedMigration :
    List Sheets.ResponseCellData
    -> Maybe String
maybeConstructAppliedMigration =
    List.head
        >> Maybe.andThen .effectiveValue
        >> Maybe.andThen
            (\value ->
                case value of
                    ResponseString string ->
                        Just string

                    _ ->
                        Nothing
            )


extractTitle : Sheets.ResponseGetSubSheetData -> Maybe String
extractTitle sheetData =
    sheetData.sheets
        |> List.head
        |> Maybe.map (.properties >> .title)



-- CONSTANTS


type Column
    = MigrationName
    | Timestamp


columnSize : Column -> Int
columnSize column =
    case column of
        MigrationName ->
            150

        Timestamp ->
            200


querySheetName : String
querySheetName =
    "query"


querySheetId : Int
querySheetId =
    0
