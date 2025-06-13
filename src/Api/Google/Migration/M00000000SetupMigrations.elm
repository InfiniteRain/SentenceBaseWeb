module Api.Google.Migration.M00000000SetupMigrations exposing
    ( Model
    , Msg
    , init
    , update
    )

import Api.Google.Constants as Constants exposing (SubSheet(..))
import Api.Google.Migration.Config as Config_
import Api.Google.Migration.Effect as Effect exposing (Effect, EffectWithPayload)
import Api.Google.Requests as Requests exposing (SheetRequestBatchUpdateKind(..))
import Api.Google.TaskCmd as TaskCmd
import Http
import Set exposing (Set)



-- MODEL


type alias Model =
    { sheetId : String
    , appliedMigrations : Set String
    }


type alias Config =
    Config_.Config Model Msg


init : String -> Config
init sheetId =
    { id = "SetupMigrations"
    , model =
        { sheetId = sheetId
        , appliedMigrations = Set.empty
        }
    , initialTask =
        Requests.getSubSheetDataRequest
            [ { sheetId = Constants.subSheetId Migrations
              , startRowIndex = Just 1
              , endRowIndex = Nothing
              , startColumnIndex = Just 0
              , endColumnIndex = Just 1
              }
            ]
            sheetId
            |> Requests.buildTask
            |> TaskCmd.attempt GotMigrationsSubSheetDataResponse
    }



-- UPDATE


type alias SubSheetDataResult =
    Result
        Requests.Error
        Requests.SheetResponseGetSubSheetData


type Msg
    = GotMigrationsSubSheetDataResponse SubSheetDataResult
    | GotCreateMigrationsSubSheetResponse (Result Requests.Error ())
    | GotQuerySubSheetDataResponse SubSheetDataResult
    | GotCreateQuerySubSheetResponse (Result Requests.Error ())
    | GotRenameQuerySubSheetEffect (Result Requests.Error ())


update : Msg -> Model -> ( Model, EffectWithPayload Msg (Set String) )
update msg ({ sheetId } as model) =
    case msg of
        GotMigrationsSubSheetDataResponse (Err (Requests.Http (Http.BadStatus 400))) ->
            ( model
            , createSubSheetEffect sheetId
                GotCreateMigrationsSubSheetResponse
                { id = 100
                , name = "migrations"
                , columns =
                    [ ( "name", MigrationName )
                    , ( "applied_at", DateTime )
                    ]
                }
            )

        GotMigrationsSubSheetDataResponse (Err err) ->
            ( model, Effect.fail err )

        GotMigrationsSubSheetDataResponse (Ok response) ->
            ( { model
                | appliedMigrations = extractAppliedMigrations response
              }
            , getQueryCellEffect sheetId
            )

        GotCreateMigrationsSubSheetResponse (Err err) ->
            ( model, Effect.fail err )

        GotCreateMigrationsSubSheetResponse (Ok _) ->
            ( model, getQueryCellEffect sheetId )

        GotQuerySubSheetDataResponse (Err (Requests.Http (Http.BadStatus 400))) ->
            ( model
            , createSubSheetEffect sheetId
                GotCreateQuerySubSheetResponse
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
                , Requests.sheetBatchUpdateRequest
                    [ UpdateSheetProperties
                        { properties =
                            { sheetId = Nothing
                            , title = Just <| querySheetName
                            , gridProperties = Nothing
                            }
                        , fields = "title"
                        }
                    ]
                    sheetId
                    |> Requests.buildTask
                    |> Effect.task GotRenameQuerySubSheetEffect
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
    String
    -> (Result Requests.Error () -> Msg)
    -> Requests.SubSheet Column
    -> EffectWithPayload Msg (Set String)
createSubSheetEffect sheetId toMsg subSheetConfig =
    Requests.sheetBatchUpdateRequest
        (Requests.addSubSheetRequests columnSize [ subSheetConfig ])
        sheetId
        |> Requests.buildTask
        |> Effect.task toMsg


getQueryCellEffect : String -> EffectWithPayload Msg (Set String)
getQueryCellEffect sheetId =
    Requests.getSubSheetDataRequest
        [ { sheetId = Constants.subSheetId Query
          , startRowIndex = Just 0
          , endRowIndex = Just 1
          , startColumnIndex = Just 0
          , endColumnIndex = Just 1
          }
        ]
        sheetId
        |> Requests.buildTask
        |> Effect.task GotQuerySubSheetDataResponse


extractAppliedMigrations : Requests.SheetResponseGetSubSheetData -> Set String
extractAppliedMigrations sheetData =
    sheetData.sheets
        |> List.head
        |> Maybe.map .data
        |> Maybe.andThen List.head
        |> Maybe.andThen .rowData
        |> Maybe.withDefault []
        |> List.map
            (.values
                >> Maybe.andThen List.head
                >> Maybe.andThen .effectiveValue
                >> Maybe.andThen
                    (\value ->
                        case value of
                            Requests.String string ->
                                Just string

                            _ ->
                                Nothing
                    )
            )
        |> List.filterMap identity
        |> Set.fromList


extractTitle : Requests.SheetResponseGetSubSheetData -> Maybe String
extractTitle sheetData =
    sheetData.sheets
        |> List.head
        |> Maybe.map (.properties >> .title)



-- CONSTANTS


type Column
    = MigrationName
    | DateTime


columnSize : Column -> Int
columnSize column =
    case column of
        MigrationName ->
            150

        DateTime ->
            200


querySheetName : String
querySheetName =
    "query"
