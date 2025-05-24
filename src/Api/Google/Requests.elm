module Api.Google.Requests exposing
    ( DriveResponseFileCreate
    , DriveResponseFileList
    , SheetRequestBatchUpdateKind(..)
    , SheetRequestDimension(..)
    , SheetRequestExtendedValue(..)
    , SheetResponseCellExtendedData(..)
    , SheetResponseGetSubSheetData
    , addSubSheetRequests
    , createAppFolderRequest
    , createMainSheetRequest
    , findAppFoldersRequest
    , findMainSheetRequest
    , getAppFolderId
    , getSubSheetDataRequest
    , httpRequest
    , httpTask
    , sheetBatchUpdateExternalRequest
    , sheetBatchUpdateRequest
    )

import Api.Google.Constants as Constants exposing (MimeType(..), SpecialFile(..))
import Api.Google.ParamTask exposing (ParamTask)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Task exposing (Task)
import Url.Builder exposing (QueryParameter, crossOrigin, string)



-- DECODERS


type alias DriveResponseFile =
    { kind : String
    , mimeType : String
    , id : String
    , name : String
    }


driveResponseFileDecoder : Decoder DriveResponseFile
driveResponseFileDecoder =
    Decode.map4 DriveResponseFile
        (Decode.field "kind" Decode.string)
        (Decode.field "mimeType" Decode.string)
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)


type alias DriveResponseFileList =
    { files : List DriveResponseFile
    }


driveResponseFileListDecoder : Decoder DriveResponseFileList
driveResponseFileListDecoder =
    Decode.map DriveResponseFileList
        (Decode.field "files" (Decode.list driveResponseFileDecoder))


type alias DriveResponseFileCreate =
    { id : String }


driveResponseFileCreateDecoder : Decoder DriveResponseFileCreate
driveResponseFileCreateDecoder =
    Decode.map DriveResponseFileCreate
        (Decode.field "id" Decode.string)


type alias SheetResponseProperties =
    { sheetId : Int
    , title : String
    }


sheetResponsePropertiesDecoder : Decoder SheetResponseProperties
sheetResponsePropertiesDecoder =
    Decode.map2 SheetResponseProperties
        (Decode.field "sheetId" Decode.int)
        (Decode.field "title" Decode.string)


type SheetResponseCellExtendedData
    = Number Float
    | String String
    | Bool Bool
    | Formula String
    | Error String


sheetResponseCellExtendedDataDecoder : Decoder SheetResponseCellExtendedData
sheetResponseCellExtendedDataDecoder =
    Decode.oneOf
        [ Decode.map Number <| Decode.field "numberValue" Decode.float
        , Decode.map String <| Decode.field "stringValue" Decode.string
        , Decode.map Bool <| Decode.field "boolValue" Decode.bool
        , Decode.map Formula <| Decode.field "stringValue" Decode.string
        , Decode.map Error <|
            Decode.field "errorValue" <|
                Decode.field "message" Decode.string
        ]


type alias SheetResponseCellData =
    { effectiveValue : Maybe SheetResponseCellExtendedData
    }


sheetResponseCellDataDecoder : Decoder SheetResponseCellData
sheetResponseCellDataDecoder =
    Decode.map SheetResponseCellData
        (Decode.maybe <|
            Decode.field "effectiveValue" sheetResponseCellExtendedDataDecoder
        )


type alias SheetResponseRowData =
    { values : Maybe (List SheetResponseCellData)
    }


sheetResponseRowDataDecoder : Decoder SheetResponseRowData
sheetResponseRowDataDecoder =
    Decode.map SheetResponseRowData
        (Decode.maybe <|
            Decode.field "values" <|
                Decode.list sheetResponseCellDataDecoder
        )


type alias SheetResponseData =
    { rowData : Maybe (List SheetResponseRowData)
    }


sheetResponseDataDecoder : Decoder SheetResponseData
sheetResponseDataDecoder =
    Decode.map SheetResponseData
        (Decode.maybe
            (Decode.field "rowData" <|
                Decode.list sheetResponseRowDataDecoder
            )
        )


type alias SheetResponse =
    { properties : SheetResponseProperties
    , data : List SheetResponseData
    }


sheetResponseDecoder : Decoder SheetResponse
sheetResponseDecoder =
    Decode.map2 SheetResponse
        (Decode.field "properties" sheetResponsePropertiesDecoder)
        (Decode.field "data" <| Decode.list sheetResponseDataDecoder)


type alias SheetResponseGetSubSheetData =
    { sheets : List SheetResponse }


sheetResponseGetSubSheetDataDecoder : Decoder SheetResponseGetSubSheetData
sheetResponseGetSubSheetDataDecoder =
    Decode.map SheetResponseGetSubSheetData
        (Decode.field "sheets" (Decode.list sheetResponseDecoder))



-- ENCODERS


type alias DriveRequestCreateFile =
    { name : String
    , mimeType : String
    , parents : Maybe (List String)
    }


driveRequestCreateFileEncoder : DriveRequestCreateFile -> Encode.Value
driveRequestCreateFileEncoder createFile =
    Encode.object <|
        List.concat
            [ [ ( "mimeType", Encode.string createFile.mimeType )
              , ( "name", Encode.string createFile.name )
              ]
            , case createFile.parents of
                Just parents ->
                    [ ( "parents", Encode.list Encode.string parents ) ]

                Nothing ->
                    []
            ]


type alias SheetRequestGridProperties =
    { frozenRowCount : Maybe Int
    , frozenColumnCount : Maybe Int
    }


sheetRequestGridPropertiesEncoder : SheetRequestGridProperties -> Encode.Value
sheetRequestGridPropertiesEncoder { frozenRowCount, frozenColumnCount } =
    Encode.object <|
        maybeListEncoder <|
            [ maybeEncoder
                ( "frozenRowCount", frozenRowCount, Encode.int )
            , maybeEncoder
                ( "frozenColumnCount", frozenColumnCount, Encode.int )
            ]


type alias SheetRequestProperties =
    { sheetId : Maybe Int
    , title : Maybe String
    , gridProperties : Maybe SheetRequestGridProperties
    }


sheetRequestPropertiesEncoder : SheetRequestProperties -> Encode.Value
sheetRequestPropertiesEncoder properties =
    Encode.object <|
        maybeListEncoder <|
            [ maybeEncoder
                ( "sheetId", properties.sheetId, Encode.int )
            , maybeEncoder
                ( "title", properties.title, Encode.string )
            , maybeEncoder
                ( "gridProperties"
                , properties.gridProperties
                , sheetRequestGridPropertiesEncoder
                )
            ]


type alias SheetRequestConditionValue =
    { userEnteredValue : String }


sheetRequestConditionValueEncoder : SheetRequestConditionValue -> Encode.Value
sheetRequestConditionValueEncoder { userEnteredValue } =
    Encode.object
        [ ( "userEnteredValue", Encode.string userEnteredValue )
        ]


type alias SheetRequestBooleanCondition =
    { type_ : String
    , values : List SheetRequestConditionValue
    }


sheetRequestBooleanConditionEncoder :
    SheetRequestBooleanCondition
    -> Encode.Value
sheetRequestBooleanConditionEncoder { type_, values } =
    Encode.object
        [ ( "type", Encode.string type_ )
        , ( "values", values |> Encode.list sheetRequestConditionValueEncoder )
        ]


type alias SheetRequestDataValidationRule =
    { condition : SheetRequestBooleanCondition
    , strict : Bool
    }


sheetRequestDataValidationRuleEncoder :
    SheetRequestDataValidationRule
    -> Encode.Value
sheetRequestDataValidationRuleEncoder { condition, strict } =
    Encode.object
        [ ( "condition", sheetRequestBooleanConditionEncoder condition )
        , ( "strict", Encode.bool strict )
        ]


type alias SheetRequestGridRange =
    { sheetId : Int
    , startRowIndex : Maybe Int
    , endRowIndex : Maybe Int
    , startColumnIndex : Maybe Int
    , endColumnIndex : Maybe Int
    }


sheetRequestGridRangeEncoder : SheetRequestGridRange -> Encode.Value
sheetRequestGridRangeEncoder range =
    Encode.object <|
        ( "sheetId", Encode.int range.sheetId )
            :: maybeListEncoder
                [ maybeEncoder
                    ( "startRowIndex", range.startRowIndex, Encode.int )
                , maybeEncoder
                    ( "endRowIndex", range.endRowIndex, Encode.int )
                , maybeEncoder
                    ( "startColumnIndex", range.startColumnIndex, Encode.int )
                , maybeEncoder
                    ( "endColumnIndex", range.endColumnIndex, Encode.int )
                ]


type alias SheetRequestDataFilter =
    { gridRange : SheetRequestGridRange
    }


sheetRequestDataFilterEncoder : SheetRequestDataFilter -> Encode.Value
sheetRequestDataFilterEncoder sheetDataFilter =
    Encode.object
        [ ( "gridRange"
          , sheetRequestGridRangeEncoder
                sheetDataFilter.gridRange
          )
        ]


type alias SheetRequestGetByDataFilter =
    { dataFilters : List SheetRequestDataFilter
    }


sheetRequestGetByDataFilterEncoder : SheetRequestGetByDataFilter -> Encode.Value
sheetRequestGetByDataFilterEncoder sheetGetByDataFilterRequest =
    Encode.object
        [ ( "dataFilters"
          , Encode.list
                sheetRequestDataFilterEncoder
                sheetGetByDataFilterRequest.dataFilters
          )
        ]


type SheetRequestExtendedValue
    = NumberValue Float
    | StringValue String
    | BoolValue Bool
    | FormulaValue String


sheetRequestExtendedValueEncoder : SheetRequestExtendedValue -> Encode.Value
sheetRequestExtendedValueEncoder value =
    Encode.object
        [ case value of
            NumberValue float ->
                ( "numberValue", Encode.float float )

            StringValue string ->
                ( "stringValue", Encode.string string )

            BoolValue bool ->
                ( "boolValue", Encode.bool bool )

            FormulaValue formulaString ->
                ( "formulaValue", Encode.string formulaString )
        ]


type alias SheetRequestCellData =
    { userEnteredValue : SheetRequestExtendedValue }


sheetRequestCellDataEncoder : SheetRequestCellData -> Encode.Value
sheetRequestCellDataEncoder { userEnteredValue } =
    Encode.object
        [ ( "userEnteredValue"
          , sheetRequestExtendedValueEncoder userEnteredValue
          )
        ]


type alias SheetRequestRowData =
    { values : List SheetRequestCellData }


sheetRequestRowDataEncoder : SheetRequestRowData -> Encode.Value
sheetRequestRowDataEncoder { values } =
    Encode.object
        [ ( "values", Encode.list sheetRequestCellDataEncoder values )
        ]


type SheetRequestDimension
    = Unspecified
    | Rows
    | Columns


sheetRequestDimensionEncoder : SheetRequestDimension -> Encode.Value
sheetRequestDimensionEncoder dimension =
    case dimension of
        Unspecified ->
            Encode.string "DIMENSION_UNSPECIFIED"

        Rows ->
            Encode.string "ROWS"

        Columns ->
            Encode.string "COLUMNS"


type alias SheetRequestDimensionRange =
    { sheetId : Int
    , dimension : SheetRequestDimension
    , startIndex : Int
    , endIndex : Int
    }


sheetRequestDimensionRangeEncoder : SheetRequestDimensionRange -> Encode.Value
sheetRequestDimensionRangeEncoder { sheetId, dimension, startIndex, endIndex } =
    Encode.object
        [ ( "sheetId", Encode.int sheetId )
        , ( "dimension", sheetRequestDimensionEncoder dimension )
        , ( "startIndex", Encode.int startIndex )
        , ( "endIndex", Encode.int endIndex )
        ]


type alias SheetRequestDimensionProperties =
    { pixelSize : Int }


sheetRequestDimensionPropertiesEncoder :
    SheetRequestDimensionProperties
    -> Encode.Value
sheetRequestDimensionPropertiesEncoder { pixelSize } =
    Encode.object [ ( "pixelSize", Encode.int pixelSize ) ]


type SheetRequestBatchUpdateKind
    = AddSheet
        { properties : SheetRequestProperties
        }
    | SetDataValidation
        { range : SheetRequestGridRange
        , rule : SheetRequestDataValidationRule
        }
    | UpdateCells
        { rows : List SheetRequestRowData
        , fields : String
        , range : SheetRequestGridRange
        }
    | AppendCells
        { sheetId : Int
        , rows : List SheetRequestRowData
        , fields : String
        }
    | UpdateDimensionProperties
        { properties : SheetRequestDimensionProperties
        , fields : String
        , range : SheetRequestDimensionRange
        }


sheetRequestBatchUpdateKindEncoder : SheetRequestBatchUpdateKind -> Encode.Value
sheetRequestBatchUpdateKindEncoder kind =
    Encode.object <|
        case kind of
            AddSheet { properties } ->
                [ ( "addSheet"
                  , Encode.object
                        [ ( "properties"
                          , sheetRequestPropertiesEncoder properties
                          )
                        ]
                  )
                ]

            SetDataValidation { range, rule } ->
                [ ( "setDataValidation"
                  , Encode.object
                        [ ( "range", sheetRequestGridRangeEncoder range )
                        , ( "rule", sheetRequestDataValidationRuleEncoder rule )
                        ]
                  )
                ]

            UpdateCells { rows, fields, range } ->
                [ ( "updateCells"
                  , Encode.object
                        [ ( "rows"
                          , Encode.list sheetRequestRowDataEncoder rows
                          )
                        , ( "fields", Encode.string fields )
                        , ( "range", sheetRequestGridRangeEncoder range )
                        ]
                  )
                ]

            AppendCells { sheetId, rows, fields } ->
                [ ( "appendCells"
                  , Encode.object
                        [ ( "sheetId", Encode.int sheetId )
                        , ( "rows"
                          , Encode.list sheetRequestRowDataEncoder rows
                          )
                        , ( "fields", Encode.string fields )
                        ]
                  )
                ]

            UpdateDimensionProperties { properties, fields, range } ->
                [ ( "updateDimensionProperties"
                  , Encode.object
                        [ ( "properties"
                          , sheetRequestDimensionPropertiesEncoder
                                properties
                          )
                        , ( "fields", Encode.string fields )
                        , ( "range", sheetRequestDimensionRangeEncoder range )
                        ]
                  )
                ]


type alias SheetRequestBatchUpdate =
    { requests : List SheetRequestBatchUpdateKind
    }


sheetRequestBatchUpdateEncoder : SheetRequestBatchUpdate -> Encode.Value
sheetRequestBatchUpdateEncoder request =
    Encode.object
        [ ( "requests"
          , Encode.list
                sheetRequestBatchUpdateKindEncoder
                request.requests
          )
        ]



-- INTERNAL API


type alias HttpTask response =
    Task Http.Error response


handleJsonResponse : Decoder a -> Http.Response String -> Result Http.Error a
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.GoodStatus_ _ body ->
            case Decode.decodeString decoder body of
                Err _ ->
                    Err (Http.BadBody body)

                Ok result ->
                    Ok result


jsonResolver : Decoder response -> Http.Resolver Http.Error response
jsonResolver decoder =
    Http.stringResolver <| handleJsonResponse <| decoder


httpTask :
    { token : String
    , method : String
    , url : String
    , body : Http.Body
    , resolver : Http.Resolver Http.Error response
    }
    -> HttpTask response
httpTask { token, method, url, body, resolver } =
    Http.task
        { method = method
        , headers = [ tokenHeader token ]
        , url = url
        , body = body
        , resolver = resolver
        , timeout = Nothing
        }


httpRequest :
    { token : String
    , method : String
    , url : String
    , body : Http.Body
    , expect : Http.Expect msg
    }
    -> Cmd msg
httpRequest { token, method, url, body, expect } =
    Http.request
        { method = method
        , headers = [ tokenHeader token ]
        , url = url
        , body = body
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }


findAppFoldersRequest : String -> HttpTask DriveResponseFileList
findAppFoldersRequest token =
    httpTask
        { token = token
        , method = "GET"
        , url =
            googleUrl
                (googleDriveRoute [ "files" ])
                [ string "q"
                    ("name = '"
                        ++ Constants.specialFileName AppFolder
                        ++ "' and mimeType = '"
                        ++ Constants.mimeTypeName Folder
                        ++ "'"
                    )
                , string "orderBy" "createdTime"
                ]
        , body = Http.emptyBody
        , resolver = jsonResolver driveResponseFileListDecoder
        }


createAppFolderRequest : String -> HttpTask DriveResponseFileCreate
createAppFolderRequest token =
    httpTask
        { token = token
        , method = "POST"
        , url = googleUrl (googleDriveRoute [ "files" ]) []
        , body =
            Http.jsonBody <|
                driveRequestCreateFileEncoder
                    { name = Constants.specialFileName AppFolder
                    , mimeType = Constants.mimeTypeName Folder
                    , parents = Nothing
                    }
        , resolver = jsonResolver driveResponseFileCreateDecoder
        }


findMainSheetRequest : String -> String -> HttpTask DriveResponseFileList
findMainSheetRequest token appFolderId =
    httpTask
        { token = token
        , method = "GET"
        , url =
            googleUrl
                (googleDriveRoute [ "files" ])
                [ string "q"
                    ("name = '"
                        ++ Constants.specialFileName MainSheet
                        ++ "' and mimeType = '"
                        ++ Constants.mimeTypeName SpreadSheet
                        ++ "' and '"
                        ++ appFolderId
                        ++ "' in parents"
                    )
                , string "orderBy" "createdTime"
                ]
        , body = Http.emptyBody
        , resolver = jsonResolver driveResponseFileListDecoder
        }


createMainSheetRequest : String -> String -> HttpTask DriveResponseFileCreate
createMainSheetRequest token appFolderId =
    httpTask
        { token = token
        , method = "POST"
        , url = googleUrl (googleDriveRoute [ "files" ]) []
        , body =
            Http.jsonBody
                (driveRequestCreateFileEncoder
                    { name = Constants.specialFileName MainSheet
                    , mimeType = Constants.mimeTypeName SpreadSheet
                    , parents = Just [ appFolderId ]
                    }
                )
        , resolver = jsonResolver driveResponseFileCreateDecoder
        }


getSubSheetDataRequest :
    String
    -> String
    -> List SheetRequestGridRange
    -> HttpTask SheetResponseGetSubSheetData
getSubSheetDataRequest token sheetId ranges =
    httpTask
        { token = token
        , method = "POST"
        , url =
            googleSheetsUrl
                (googleSheetsRoute [ sheetId ++ ":getByDataFilter" ])
                [ string
                    "fields"
                    "sheets(properties(sheetId,title),data.rowData.values.effectiveValue)"
                ]
        , body =
            Http.jsonBody
                (sheetRequestGetByDataFilterEncoder
                    { dataFilters =
                        ranges
                            |> List.map (\range -> { gridRange = range })
                    }
                )
        , resolver = jsonResolver sheetResponseGetSubSheetDataDecoder
        }


sheetBatchUpdateRequest :
    String
    -> String
    -> List SheetRequestBatchUpdateKind
    -> HttpTask ()
sheetBatchUpdateRequest token sheetId kinds =
    httpTask
        { token = token
        , method = "POST"
        , url =
            googleSheetsUrl
                (googleSheetsRoute [ sheetId ++ ":batchUpdate" ])
                []
        , body =
            Http.jsonBody <|
                sheetRequestBatchUpdateEncoder
                    { requests = kinds }
        , resolver = jsonResolver <| Decode.succeed ()
        }



-- EXTERNAL API


type alias HttpParamTask response =
    ParamTask Http.Error response


getAppFolderId : ParamTask Http.Error DriveResponseFileList
getAppFolderId =
    \token _ ->
        httpTask
            { token = token
            , method = "GET"
            , url =
                googleUrl
                    (googleDriveRoute [ "files" ])
                    [ string "q"
                        ("name = '"
                            ++ Constants.specialFileName AppFolder
                            ++ "' and mimeType = '"
                            ++ Constants.mimeTypeName Folder
                            ++ "'"
                        )
                    ]
            , body = Http.emptyBody
            , resolver = jsonResolver driveResponseFileListDecoder
            }


sheetBatchUpdateExternalRequest :
    List SheetRequestBatchUpdateKind
    -> HttpParamTask ()
sheetBatchUpdateExternalRequest kinds =
    \token sheetId ->
        sheetBatchUpdateRequest token sheetId kinds



-- BUILDERS


googleUrl : List String -> List QueryParameter -> String
googleUrl segments params =
    crossOrigin "https://www.googleapis.com" segments params


googleDriveRoute : List String -> List String
googleDriveRoute additional =
    [ "drive", "v3" ] ++ additional


tokenHeader : String -> Http.Header
tokenHeader token =
    Http.header "Authorization" ("Bearer " ++ token)


googleSheetsUrl : List String -> List QueryParameter -> String
googleSheetsUrl segments params =
    crossOrigin "https://sheets.googleapis.com" segments params


googleSheetsRoute : List String -> List String
googleSheetsRoute additional =
    [ "v4", "spreadsheets" ] ++ additional


maybeListEncoder :
    List (Maybe ( String, Encode.Value ))
    -> List ( String, Encode.Value )
maybeListEncoder entries =
    List.filterMap identity entries


maybeEncoder :
    ( String, Maybe a, a -> Encode.Value )
    -> Maybe ( String, Encode.Value )
maybeEncoder ( key, maybe, encoder ) =
    Maybe.map (\value -> ( key, encoder value )) maybe



-- BUILDERS


type alias SubSheet column =
    { id : Int
    , name : String
    , columns : List ( String, column )
    }


addSubSheetRequests :
    (column -> Int)
    -> List (SubSheet column)
    -> List SheetRequestBatchUpdateKind
addSubSheetRequests columnSize subSheets =
    List.concat
        [ List.concatMap
            (\subSheet ->
                [ AddSheet
                    { properties =
                        { sheetId = Just subSheet.id
                        , title = Just subSheet.name
                        , gridProperties =
                            Just
                                { frozenRowCount = Just 1
                                , frozenColumnCount = Just 0
                                }
                        }
                    }
                , UpdateCells
                    { rows =
                        [ { values =
                                List.map
                                    (\( name, _ ) ->
                                        { userEnteredValue = StringValue name }
                                    )
                                    subSheet.columns
                          }
                        ]
                    , fields = "userEnteredValue"
                    , range =
                        { sheetId = subSheet.id
                        , startRowIndex = Just 0
                        , startColumnIndex = Just 0
                        , endRowIndex = Just 1
                        , endColumnIndex = Just <| List.length subSheet.columns
                        }
                    }
                , SetDataValidation
                    { range =
                        { sheetId = subSheet.id
                        , startRowIndex = Just 1
                        , endRowIndex = Nothing
                        , startColumnIndex = Just 0
                        , endColumnIndex = Just <| List.length subSheet.columns
                        }
                    , rule =
                        { condition =
                            { type_ = "CUSTOM_FORMULA"
                            , values =
                                [ { userEnteredValue =
                                        validationFormula subSheet.columns
                                  }
                                ]
                            }
                        , strict = True
                        }
                    }
                ]
            )
            subSheets
        , List.concatMap
            (\subSheet ->
                List.indexedMap
                    (\index ( _, column ) ->
                        UpdateDimensionProperties
                            { properties =
                                { pixelSize = columnSize column
                                }
                            , fields = "pixelSize"
                            , range =
                                { sheetId = subSheet.id
                                , dimension = Columns
                                , startIndex = index
                                , endIndex = index + 1
                                }
                            }
                    )
                    subSheet.columns
            )
            subSheets
        ]


validationFormula : List ( String, column ) -> String
validationFormula columns =
    "=AND("
        ++ (String.join "," <|
                List.indexedMap
                    (\index _ ->
                        "NOT(ISBLANK($"
                            ++ (Char.toCode 'A'
                                    + index
                                    |> Char.fromCode
                                    |> String.fromChar
                               )
                            ++ "2))"
                    )
                    columns
           )
        ++ ")"
