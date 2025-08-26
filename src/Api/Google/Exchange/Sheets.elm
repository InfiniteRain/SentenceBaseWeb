module Api.Google.Exchange.Sheets exposing
    ( RequestBatchUpdateKind(..)
    , RequestBooleanCondition
    , RequestCellData
    , RequestConditionValue
    , RequestDataValidationRule
    , RequestDimension(..)
    , RequestDimensionProperties
    , RequestDimensionRange
    , RequestExtendedValue(..)
    , RequestGridProperties
    , RequestGridRange
    , RequestProperties
    , RequestRowData
    , Response
    , ResponseBatchUpdate
    , ResponseCellData
    , ResponseCellExtendedData(..)
    , ResponseData
    , ResponseGetSubSheetData
    , ResponseGridProperties
    , ResponseProperties
    , ResponseRowData
    , SubSheet
    , addSubSheetRequests
    , batchUpdateAndGetGridDataRequest
    , batchUpdateRequest
    , getSubSheetDataRequest
    , requestRow
    , requestRows
    , tagsExtendedValue
    , timestampExtendedValue
    )

import Api.Google.Exchange.Task as Task
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Time
import Url.Builder exposing (QueryParameter, crossOrigin, string)



-- DECODERS


type alias ResponseGridProperties =
    { columnCount : Int }


responseGridPropertiesDecoder : Decoder ResponseGridProperties
responseGridPropertiesDecoder =
    Decode.map ResponseGridProperties
        (Decode.field "columnCount" Decode.int)


type alias ResponseProperties =
    { gridProperties : ResponseGridProperties
    , sheetId : Int
    , title : String
    }


responsePropertiesDecoder : Decoder ResponseProperties
responsePropertiesDecoder =
    Decode.map3 ResponseProperties
        (Decode.field "gridProperties" responseGridPropertiesDecoder)
        (Decode.field "sheetId" Decode.int)
        (Decode.field "title" Decode.string)


type ResponseCellExtendedData
    = ResponseNumber Float
    | ResponseString String
    | ResponseBool Bool
    | ResponseFormula String


responseCellExtendedDataDecoder : Decoder ResponseCellExtendedData
responseCellExtendedDataDecoder =
    Decode.oneOf
        [ Decode.map ResponseNumber <| Decode.field "numberValue" Decode.float
        , Decode.map ResponseString <| Decode.field "stringValue" Decode.string
        , Decode.map ResponseBool <| Decode.field "boolValue" Decode.bool
        , Decode.map ResponseFormula <| Decode.field "stringValue" Decode.string
        ]


type alias ResponseCellData =
    { effectiveValue : Maybe ResponseCellExtendedData
    }


responseCellDataDecoder : Decoder ResponseCellData
responseCellDataDecoder =
    Decode.map ResponseCellData
        (Decode.maybe <|
            Decode.field "effectiveValue" responseCellExtendedDataDecoder
        )


type alias ResponseRowData =
    { values : Maybe (List ResponseCellData)
    }


responseRowDataDecoder : Decoder ResponseRowData
responseRowDataDecoder =
    Decode.map ResponseRowData
        (Decode.maybe <|
            Decode.field "values" <|
                Decode.list responseCellDataDecoder
        )


type alias ResponseData =
    { rowData : Maybe (List ResponseRowData)
    }


responseDataDecoder : Decoder ResponseData
responseDataDecoder =
    Decode.map ResponseData
        (Decode.maybe
            (Decode.field "rowData" <|
                Decode.list responseRowDataDecoder
            )
        )


type alias Response =
    { properties : ResponseProperties
    , data : List ResponseData
    }


responseDecoder : Decoder Response
responseDecoder =
    Decode.map2 Response
        (Decode.field "properties" responsePropertiesDecoder)
        (Decode.field "data" <| Decode.list responseDataDecoder)


type alias ResponseGetSubSheetData =
    { sheets : List Response }


responseGetSubSheetDataDecoder : Decoder ResponseGetSubSheetData
responseGetSubSheetDataDecoder =
    Decode.map ResponseGetSubSheetData
        (Decode.field "sheets" (Decode.list responseDecoder))


type alias ResponseBatchUpdate =
    { updatedSpreadsheet : ResponseGetSubSheetData }


responseBatchUpdateDecoder : Decoder ResponseBatchUpdate
responseBatchUpdateDecoder =
    Decode.map ResponseBatchUpdate
        (Decode.field "updatedSpreadsheet" responseGetSubSheetDataDecoder)



-- ENCODERS


type alias RequestGridProperties =
    { frozenRowCount : Maybe Int
    , frozenColumnCount : Maybe Int
    }


requestGridPropertiesEncoder : RequestGridProperties -> Encode.Value
requestGridPropertiesEncoder { frozenRowCount, frozenColumnCount } =
    Encode.object <|
        maybeListEncoder <|
            [ maybeEncoder
                ( "frozenRowCount", frozenRowCount, Encode.int )
            , maybeEncoder
                ( "frozenColumnCount", frozenColumnCount, Encode.int )
            ]


type alias RequestProperties =
    { sheetId : Maybe Int
    , title : Maybe String
    , gridProperties : Maybe RequestGridProperties
    }


requestPropertiesEncoder : RequestProperties -> Encode.Value
requestPropertiesEncoder properties =
    Encode.object <|
        maybeListEncoder <|
            [ maybeEncoder
                ( "sheetId", properties.sheetId, Encode.int )
            , maybeEncoder
                ( "title", properties.title, Encode.string )
            , maybeEncoder
                ( "gridProperties"
                , properties.gridProperties
                , requestGridPropertiesEncoder
                )
            ]


type alias RequestConditionValue =
    { userEnteredValue : String }


requestConditionValueEncoder : RequestConditionValue -> Encode.Value
requestConditionValueEncoder { userEnteredValue } =
    Encode.object
        [ ( "userEnteredValue", Encode.string userEnteredValue )
        ]


type alias RequestBooleanCondition =
    { type_ : String
    , values : List RequestConditionValue
    }


requestBooleanConditionEncoder :
    RequestBooleanCondition
    -> Encode.Value
requestBooleanConditionEncoder { type_, values } =
    Encode.object
        [ ( "type", Encode.string type_ )
        , ( "values", values |> Encode.list requestConditionValueEncoder )
        ]


type alias RequestDataValidationRule =
    { condition : RequestBooleanCondition
    , strict : Bool
    }


requestDataValidationRuleEncoder :
    RequestDataValidationRule
    -> Encode.Value
requestDataValidationRuleEncoder { condition, strict } =
    Encode.object
        [ ( "condition", requestBooleanConditionEncoder condition )
        , ( "strict", Encode.bool strict )
        ]


type alias RequestGridRange =
    { sheetId : Int
    , startRowIndex : Maybe Int
    , endRowIndex : Maybe Int
    , startColumnIndex : Maybe Int
    , endColumnIndex : Maybe Int
    }


requestGridRangeEncoder : RequestGridRange -> Encode.Value
requestGridRangeEncoder range =
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


type alias RequestDataFilter =
    { gridRange : RequestGridRange
    }


requestDataFilterEncoder : RequestDataFilter -> Encode.Value
requestDataFilterEncoder sheetDataFilter =
    Encode.object
        [ ( "gridRange"
          , requestGridRangeEncoder
                sheetDataFilter.gridRange
          )
        ]


type alias RequestGetByDataFilter =
    { dataFilters : List RequestDataFilter
    }


requestGetByDataFilterEncoder : RequestGetByDataFilter -> Encode.Value
requestGetByDataFilterEncoder sheetGetByDataFilterRequest =
    Encode.object
        [ ( "dataFilters"
          , Encode.list
                requestDataFilterEncoder
                sheetGetByDataFilterRequest.dataFilters
          )
        ]


type RequestExtendedValue
    = RequestNumber Float
    | RequestString String
    | RequestFormula String


requestExtendedValueEncoder : RequestExtendedValue -> Encode.Value
requestExtendedValueEncoder value =
    Encode.object
        [ case value of
            RequestNumber float ->
                ( "numberValue", Encode.float float )

            RequestString string ->
                ( "stringValue", Encode.string string )

            RequestFormula formulaString ->
                ( "formulaValue", Encode.string formulaString )
        ]


type alias RequestCellData =
    { userEnteredValue : RequestExtendedValue }


requestCellDataEncoder : RequestCellData -> Encode.Value
requestCellDataEncoder { userEnteredValue } =
    Encode.object
        [ ( "userEnteredValue"
          , requestExtendedValueEncoder userEnteredValue
          )
        ]


type alias RequestRowData =
    { values : List RequestCellData }


requestRowDataEncoder : RequestRowData -> Encode.Value
requestRowDataEncoder { values } =
    Encode.object
        [ ( "values", Encode.list requestCellDataEncoder values )
        ]


type RequestDimension
    = RequestRows
    | RequestColumns


requestDimensionEncoder : RequestDimension -> Encode.Value
requestDimensionEncoder dimension =
    case dimension of
        RequestRows ->
            Encode.string "ROWS"

        RequestColumns ->
            Encode.string "COLUMNS"


type alias RequestDimensionRange =
    { sheetId : Int
    , dimension : RequestDimension
    , startIndex : Int
    , endIndex : Int
    }


requestDimensionRangeEncoder : RequestDimensionRange -> Encode.Value
requestDimensionRangeEncoder { sheetId, dimension, startIndex, endIndex } =
    Encode.object
        [ ( "sheetId", Encode.int sheetId )
        , ( "dimension", requestDimensionEncoder dimension )
        , ( "startIndex", Encode.int startIndex )
        , ( "endIndex", Encode.int endIndex )
        ]


type alias RequestDimensionProperties =
    { pixelSize : Int }


requestDimensionPropertiesEncoder :
    RequestDimensionProperties
    -> Encode.Value
requestDimensionPropertiesEncoder { pixelSize } =
    Encode.object [ ( "pixelSize", Encode.int pixelSize ) ]


type RequestBatchUpdateKind
    = RequestAddSheet
        { properties : RequestProperties
        }
    | RequestSetDataValidation
        { range : RequestGridRange
        , rule : RequestDataValidationRule
        }
    | RequestUpdateCells
        { rows : List RequestRowData
        , fields : String
        , range : RequestGridRange
        }
    | RequestAppendCells
        { sheetId : Int
        , rows : List RequestRowData
        , fields : String
        }
    | RequestUpdateDimensionProperties
        { properties : RequestDimensionProperties
        , fields : String
        , range : RequestDimensionRange
        }
    | RequestDeleteRange
        { range : RequestGridRange
        , dimension : RequestDimension
        }
    | RequestUpdateSheetProperties
        { properties : RequestProperties
        , fields : String
        }
    | RequestInsertDimension
        { range : RequestDimensionRange
        , inheritFromBefore : Bool
        }
    | RequestDeleteDuplicates
        { range : RequestGridRange
        , comparisonColumns : List RequestDimensionRange
        }
    | RequestAppendDimension
        { sheetId : Int
        , dimension : RequestDimension
        , length : Int
        }


requestBatchUpdateKindEncoder : RequestBatchUpdateKind -> Encode.Value
requestBatchUpdateKindEncoder kind =
    Encode.object <|
        case kind of
            RequestAddSheet { properties } ->
                [ ( "addSheet"
                  , Encode.object
                        [ ( "properties"
                          , requestPropertiesEncoder properties
                          )
                        ]
                  )
                ]

            RequestSetDataValidation { range, rule } ->
                [ ( "setDataValidation"
                  , Encode.object
                        [ ( "range", requestGridRangeEncoder range )
                        , ( "rule", requestDataValidationRuleEncoder rule )
                        ]
                  )
                ]

            RequestUpdateCells { rows, fields, range } ->
                [ ( "updateCells"
                  , Encode.object
                        [ ( "rows"
                          , Encode.list requestRowDataEncoder rows
                          )
                        , ( "fields", Encode.string fields )
                        , ( "range", requestGridRangeEncoder range )
                        ]
                  )
                ]

            RequestAppendCells { sheetId, rows, fields } ->
                [ ( "appendCells"
                  , Encode.object
                        [ ( "sheetId", Encode.int sheetId )
                        , ( "rows"
                          , Encode.list requestRowDataEncoder rows
                          )
                        , ( "fields", Encode.string fields )
                        ]
                  )
                ]

            RequestUpdateDimensionProperties { properties, fields, range } ->
                [ ( "updateDimensionProperties"
                  , Encode.object
                        [ ( "properties"
                          , requestDimensionPropertiesEncoder
                                properties
                          )
                        , ( "fields", Encode.string fields )
                        , ( "range", requestDimensionRangeEncoder range )
                        ]
                  )
                ]

            RequestDeleteRange { range, dimension } ->
                [ ( "deleteRange"
                  , Encode.object
                        [ ( "range", requestGridRangeEncoder range )
                        , ( "shiftDimension"
                          , requestDimensionEncoder
                                dimension
                          )
                        ]
                  )
                ]

            RequestUpdateSheetProperties { properties, fields } ->
                [ ( "updateSheetProperties"
                  , Encode.object
                        [ ( "properties"
                          , requestPropertiesEncoder properties
                          )
                        , ( "fields", Encode.string fields )
                        ]
                  )
                ]

            RequestInsertDimension { range, inheritFromBefore } ->
                [ ( "insertDimension"
                  , Encode.object
                        [ ( "range", requestDimensionRangeEncoder range )
                        , ( "inheritFromBefore", Encode.bool inheritFromBefore )
                        ]
                  )
                ]

            RequestDeleteDuplicates { range, comparisonColumns } ->
                [ ( "deleteDuplicates"
                  , Encode.object
                        [ ( "range", requestGridRangeEncoder range )
                        , ( "comparisonColumns"
                          , Encode.list
                                requestDimensionRangeEncoder
                                comparisonColumns
                          )
                        ]
                  )
                ]

            RequestAppendDimension { sheetId, dimension, length } ->
                [ ( "appendDimension"
                  , Encode.object
                        [ ( "sheetId", Encode.int sheetId )
                        , ( "dimension", requestDimensionEncoder dimension )
                        , ( "length", Encode.int length )
                        ]
                  )
                ]


type alias RequestBatchUpdate =
    { requests : List RequestBatchUpdateKind
    , includeSpreadsheetInResponse : Bool
    , responseRanges : List String
    , responseIncludeGridData : Bool
    }


requestBatchUpdateEncoder : RequestBatchUpdate -> Encode.Value
requestBatchUpdateEncoder request =
    Encode.object
        [ ( "requests"
          , Encode.list
                requestBatchUpdateKindEncoder
                request.requests
          )
        , ( "includeSpreadsheetInResponse"
          , Encode.bool
                request.includeSpreadsheetInResponse
          )
        , ( "responseRanges", Encode.list Encode.string request.responseRanges )
        , ( "responseIncludeGridData"
          , Encode.bool
                request.responseIncludeGridData
          )
        ]



-- API


getSubSheetDataRequest :
    List RequestGridRange
    -> Task.SheetsTask ResponseGetSubSheetData
getSubSheetDataRequest ranges =
    Task.sheetsHttp
        (\sheetId ->
            { method = "POST"
            , url =
                googleSheetsUrl
                    (googleSheetsRoute [ sheetId ++ ":getByDataFilter" ])
                    [ string
                        "fields"
                        ("sheets"
                            ++ "(properties"
                            ++ "(sheetId,title,gridProperties.columnCount)"
                            ++ ",data.rowData.values.effectiveValue)"
                        )
                    ]
            , body =
                Http.jsonBody
                    (requestGetByDataFilterEncoder
                        { dataFilters =
                            ranges
                                |> List.map (\range -> { gridRange = range })
                        }
                    )
            , resolver = Task.Json responseGetSubSheetDataDecoder
            }
        )


batchUpdateRequest :
    List RequestBatchUpdateKind
    -> Task.SheetsTask ()
batchUpdateRequest kinds =
    Task.sheetsHttp
        (\sheetId ->
            { method = "POST"
            , url =
                googleSheetsUrl
                    (googleSheetsRoute [ sheetId ++ ":batchUpdate" ])
                    []
            , body =
                Http.jsonBody <|
                    requestBatchUpdateEncoder
                        { requests = kinds
                        , includeSpreadsheetInResponse = False
                        , responseRanges = []
                        , responseIncludeGridData = False
                        }
            , resolver = Task.Custom ()
            }
        )


batchUpdateAndGetGridDataRequest :
    List RequestBatchUpdateKind
    -> List String
    -> Task.SheetsTask ResponseBatchUpdate
batchUpdateAndGetGridDataRequest kinds responseRanges =
    Task.sheetsHttp
        (\sheetId ->
            { method = "POST"
            , url =
                googleSheetsUrl
                    (googleSheetsRoute [ sheetId ++ ":batchUpdate" ])
                    [ string
                        "fields"
                        ("updatedSpreadsheet.sheets"
                            ++ "(properties"
                            ++ "(sheetId,title,gridProperties.columnCount)"
                            ++ ",data.rowData.values.effectiveValue)"
                        )
                    ]
            , body =
                Http.jsonBody <|
                    requestBatchUpdateEncoder
                        { requests = kinds
                        , includeSpreadsheetInResponse = True
                        , responseRanges = responseRanges
                        , responseIncludeGridData = True
                        }
            , resolver = Task.Json responseBatchUpdateDecoder
            }
        )



-- BUILDERS


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


type alias SubSheet column =
    { id : Int
    , name : String
    , columns : List ( String, column )
    , additionalColumnsCount : Int
    }


addSubSheetRequests :
    (column -> Int)
    -> List (SubSheet column)
    -> List RequestBatchUpdateKind
addSubSheetRequests columnSize subSheets =
    List.concat
        [ List.concatMap
            (\subSheet ->
                List.concat
                    [ [ RequestAddSheet
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
                      , RequestUpdateCells
                            { rows =
                                [ { values =
                                        List.map
                                            (\( name, _ ) ->
                                                { userEnteredValue =
                                                    RequestString name
                                                }
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
                      , RequestSetDataValidation
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
                    , if subSheet.additionalColumnsCount > 0 then
                        [ RequestAppendDimension
                            { sheetId = subSheet.id
                            , dimension = RequestColumns
                            , length = subSheet.additionalColumnsCount
                            }
                        ]

                      else
                        []
                    ]
            )
            subSheets
        , List.concatMap
            (\subSheet ->
                List.indexedMap
                    (\index ( _, column ) ->
                        RequestUpdateDimensionProperties
                            { properties =
                                { pixelSize = columnSize column
                                }
                            , fields = "pixelSize"
                            , range =
                                { sheetId = subSheet.id
                                , dimension = RequestColumns
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


requestRow : List RequestExtendedValue -> List RequestRowData
requestRow values =
    requestRows [ values ]


requestRows :
    List (List RequestExtendedValue)
    -> List RequestRowData
requestRows rows =
    List.map
        (\values ->
            { values =
                List.map
                    (\value ->
                        { userEnteredValue = value }
                    )
                    values
            }
        )
        rows



-- HELPERS


tagsExtendedValue : List String -> RequestExtendedValue
tagsExtendedValue tags =
    RequestString <| encodeTags tags


timestampExtendedValue : Time.Posix -> RequestExtendedValue
timestampExtendedValue time =
    Time.posixToMillis time
        |> toFloat
        |> RequestNumber


encodeTags : List String -> String
encodeTags tags =
    Encode.list Encode.string tags
        |> Encode.encode 0
