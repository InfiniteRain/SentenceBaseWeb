module Page.Batches exposing (Model, Msg(..), init, subscriptions, update, view)

import Api.Google.Constants as Constants exposing (SubSheet(..))
import Api.Google.Exchange.Sheets as Sheets exposing (RequestBatchUpdateKind(..), RequestDimension(..), RequestExtendedValue(..), requestRow)
import Api.Google.Exchange.Task as Task
import Api.Google.ListConstructor exposing (cellNumberValue, cellStringValue, constructFromList, extract, field)
import Api.Google.Model as Model
import Effect exposing (Effect)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Session exposing (Session)
import Set
import Time



-- MODEL


type alias Model =
    { session : Session }


init : Session -> ( Model, Cmd Msg, Effect Msg )
init session =
    ( { session = session }
    , Cmd.none
    , Effect.none
    )



-- UPDATE


type Msg
    = Foo
    | Bar (Result Task.Error ())


update : Msg -> Model -> ( Model, Cmd Msg, Effect Msg )
update msg model =
    case msg of
        Foo ->
            ( model
            , Cmd.none
            , getBatchesRequest 0
                |> Task.sheetsAttempt Bar
                |> Effect.google
            )

        Bar _ ->
            ( model, Cmd.none, Effect.none )


getBatchesRequest : Int -> Task.SheetsTask ()
getBatchesRequest page =
    Sheets.batchUpdateAndGetGridDataRequest
        [ RequestDeleteRange
            { range =
                { sheetId = Constants.subSheetId Query
                , startRowIndex = Just 0
                , endRowIndex = Nothing
                , startColumnIndex = Just 0
                , endColumnIndex = Nothing
                }
            , dimension = RequestRows
            }
        , RequestUpdateCells
            { rows =
                requestRow
                    [ RequestFormula <|
                        "=QUERY("
                            ++ Constants.subSheetName MinedSentences
                            ++ "!A2:E,\""
                            ++ "SELECT COUNT(D), D WHERE D <> '' GROUP BY D LIMIT "
                            ++ String.fromInt pageSize
                            ++ " OFFSET "
                            ++ (String.fromInt <| page * pageSize)
                            ++ "\")"
                    ]
            , fields = "userEnteredValue"
            , range =
                { sheetId = Constants.subSheetId Query
                , startRowIndex = Just 0
                , endRowIndex = Just 1
                , startColumnIndex = Just 0
                , endColumnIndex = Just 1
                }
            }
        ]
        [ Constants.subSheetName Query ++ "!B2:B" ]
        |> Task.map
            (.updatedSpreadsheet >> Model.fromGridData constructBatchIds)
        |> Task.map
            (\ids ->
                -- let
                --     _ =
                --         Debug.log "ids" ids
                -- in
                ()
            )


constructBatchIds : List Sheets.ResponseCellData -> Maybe String
constructBatchIds =
    List.head >> Maybe.andThen cellStringValue



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Batches"
    , content =
        div []
            [ text "Batches"
            , button [ onClick Foo ] [ text "click me" ]
            ]
    }



-- CONSTANTS


pageSize : Int
pageSize =
    10
