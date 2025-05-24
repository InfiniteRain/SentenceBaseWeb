module Api.Google.Migration.M00000000SetupMigrations exposing
    ( Model
    , Msg
    , init
    , update
    )

import Api.Google.Constants as Constants exposing (SubSheet(..))
import Api.Google.Migration.Config as Config_
import Api.Google.Migration.Effect as Effect exposing (EffectWithPayload)
import Api.Google.Requests as Requests
import Api.Google.TaskCmd as TaskCmd
import Http
import Set exposing (Set)



-- MODEL


type alias Model =
    { token : String
    , sheetId : String
    }


type alias Config =
    Config_.Config Model Msg


init : String -> String -> Config
init token sheetId =
    { id = "SetupMigrations"
    , model =
        { token = token
        , sheetId = sheetId
        }
    , initialTask =
        TaskCmd.attempt GotSubSheetDataResponse <|
            Requests.getSubSheetDataRequest
                token
                sheetId
                [ { sheetId = Constants.subSheetId Migrations
                  , startRowIndex = Just 1
                  , endRowIndex = Nothing
                  , startColumnIndex = Just 0
                  , endColumnIndex = Just 1
                  }
                ]
    }



-- UPDATE


type Msg
    = GotSubSheetDataResponse
        (Result
            Http.Error
            Requests.SheetResponseGetSubSheetData
        )
    | GotCreateMigrationsSubSheetResponse (Result Http.Error ())


update : Msg -> Model -> ( Model, EffectWithPayload Msg (Set String) )
update msg ({ token, sheetId } as model) =
    case msg of
        GotSubSheetDataResponse (Err (Http.BadStatus 400)) ->
            ( model, createMigrationsSubSheetEffect token sheetId )

        GotSubSheetDataResponse (Err err) ->
            ( model, Effect.fail err )

        GotSubSheetDataResponse (Ok response) ->
            ( model, Effect.doneWithPayload <| extractAppliedMigrations response )

        GotCreateMigrationsSubSheetResponse (Err err) ->
            ( model, Effect.fail err )

        GotCreateMigrationsSubSheetResponse (Ok _) ->
            ( model, Effect.doneWithPayload Set.empty )


createMigrationsSubSheetEffect :
    String
    -> String
    -> EffectWithPayload Msg (Set String)
createMigrationsSubSheetEffect token sheetId =
    Effect.task GotCreateMigrationsSubSheetResponse <|
        Requests.sheetBatchUpdateRequest
            token
            sheetId
            (Requests.addSubSheetRequests columnSize
                [ { id = 100
                  , name = "migrations"
                  , columns =
                        [ ( "name", MigrationName )
                        , ( "applied_at", DateTime )
                        ]
                  }
                ]
            )


extractAppliedMigrations : Requests.SheetResponseGetSubSheetData -> Set String
extractAppliedMigrations sheetData =
    sheetData.sheets
        |> List.head
        |> Maybe.map .data
        |> Maybe.andThen List.head
        |> Maybe.andThen .rowData
        |> Maybe.withDefault []
        |> List.map
            (\{ values } ->
                values
                    |> Maybe.andThen List.head
                    |> Maybe.andThen .effectiveValue
                    |> Maybe.andThen
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
