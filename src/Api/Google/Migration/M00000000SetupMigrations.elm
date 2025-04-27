module Api.Google.Migration.M00000000SetupMigrations exposing
    ( Model
    , Msg
    , init
    , update
    )

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
            Requests.getSubSheetData
                token
                sheetId
                [ { sheetId = Requests.subSheetIds.migrations
                  , startRowIndex = Just 1
                  , endRowIndex = Nothing
                  , startColumnIndex = Just 1
                  , endColumnIndex = Just 2
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
        Requests.sheetBatchUpdate
            token
            sheetId
            [ Requests.AddSheet
                { properties =
                    { sheetId = Just Requests.subSheetIds.migrations
                    , title = Just Requests.subSheetNames.migrations
                    , gridProperties =
                        Just
                            { frozenRowCount = Just 1
                            , frozenColumnCount = Just 0
                            }
                    }
                }
            , Requests.UpdateCells
                { rows =
                    [ { values =
                            [ { userEnteredValue =
                                    Requests.StringValue "date"
                              }
                            , { userEnteredValue =
                                    Requests.StringValue "name"
                              }
                            ]
                      }
                    ]
                , fields = "userEnteredValue"
                , range =
                    { sheetId = Requests.subSheetIds.migrations
                    , startRowIndex = Just 0
                    , startColumnIndex = Just 0
                    , endRowIndex = Just 1
                    , endColumnIndex = Just 2
                    }
                }
            , Requests.SetDataValidation
                { range =
                    { sheetId = Requests.subSheetIds.migrations
                    , startRowIndex = Just 1
                    , endRowIndex = Nothing
                    , startColumnIndex = Just 0
                    , endColumnIndex = Just 2
                    }
                , rule =
                    { condition =
                        { type_ = "CUSTOM_FORMULA"
                        , values =
                            [ { userEnteredValue =
                                    "=AND(ISDATE($A2),NOT(ISBLANK($B2)))"
                              }
                            ]
                        }
                    , strict = True
                    }
                }
            ]


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
