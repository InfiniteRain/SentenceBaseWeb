module Page.Batches exposing (Model, Msg(..), init, subscriptions, update, view)

import Api.Action as Action exposing (Action)
import Api.Google.Constants as Constants exposing (SubSheet(..))
import Api.Google.Exchange.Sheets as Sheets exposing (RequestBatchUpdateKind(..), RequestDimension(..), RequestExtendedValue(..), requestRow)
import Api.Google.Exchange.Task as Task
import Api.Google.ListConstructor exposing (cellStringValue, constructFromList, extract, field)
import Api.Google.Model as Model
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Iso8601
import Json.Decode as Decode
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session }


init : Session -> ( Model, Cmd Msg, Action Msg )
init session =
    ( { session = session }
    , Cmd.none
    , Action.none
    )



-- UPDATE


type Msg
    = Foo
    | Bar (Result Task.Error ())


update : Msg -> Model -> ( Model, Cmd Msg, Action Msg )
update msg model =
    case msg of
        Foo ->
            ( model
            , Cmd.none
            , getBatchesRequest 0
                |> Task.sheetsAttempt Bar
                |> Action.google
            )

        Bar _ ->
            ( model, Cmd.none, Action.none )


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
                ()
            )


constructBatchIds : List Sheets.ResponseCellData -> Maybe String
constructBatchIds =
    List.head >> Maybe.andThen cellStringValue


maybeConstructMinedSentence :
    List Sheets.ResponseCellData
    -> Maybe Model.MinedSentence
maybeConstructMinedSentence =
    constructFromList Model.MinedSentence
        >> field cellStringValue
        >> field cellStringValue
        >> field
            (cellStringValue
                >> Maybe.map (Decode.decodeString (Decode.list Decode.string))
                >> Maybe.andThen Result.toMaybe
            )
        >> field cellStringValue
        >> field
            (cellStringValue
                >> Maybe.map Iso8601.toTime
                >> Maybe.andThen Result.toMaybe
            )
        >> extract



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
