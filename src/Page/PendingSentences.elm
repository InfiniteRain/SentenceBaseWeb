module Page.PendingSentences exposing
    ( Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

import Api.Action as Action exposing (Action)
import Api.Google.Constants as Constants exposing (SubSheet(..))
import Api.Google.Exchange.Sheets as Sheets
    exposing
        ( RequestBatchUpdateKind(..)
        , RequestDimension(..)
        , RequestExtendedValue(..)
        )
import Api.Google.Exchange.Task as Task
import Api.Google.ListConstructor
    exposing
        ( cellStringValue
        , constructFromList
        , extract
        , field
        )
import Api.Google.Model as Model
import Array exposing (Array)
import Html exposing (Html, button, div, hr, li, span, text, ul)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)
import Iso8601
import Json.Decode as Decode
import Session exposing (Session)
import Time



-- MODEL


type alias Model =
    { session : Session
    , pendingSentences : Array Model.PendingSentence
    , selectedSentences : Array Int
    , deselectedSentences : Array Int
    , confirmBatchRequesState : ConfirmBatchRequesState
    }


type ConfirmBatchRequesState
    = Idle
    | Loading


init : Session -> ( Model, Cmd Msg, Action Msg )
init session =
    ( { session = session
      , pendingSentences = Array.empty
      , selectedSentences = Array.empty
      , deselectedSentences = Array.empty
      , confirmBatchRequesState = Idle
      }
    , Cmd.none
    , Task.sheetsAttempt ReceivedPendingSentencesList getPendingSentencesRequest
        |> Action.google
    )



-- UPDATE


type Msg
    = ReceivedPendingSentencesList
        (Result
            Task.Error
            (Array Model.PendingSentence)
        )
    | Selected Int
    | Deselected Int
    | ConfirmBatchClicked
    | UuidReceived String
    | BatchConfirmed (Result Task.Error ())


update : Msg -> Model -> ( Model, Cmd Msg, Action Msg )
update msg model =
    case msg of
        ReceivedPendingSentencesList (Ok pendingSentences) ->
            ( { model
                | pendingSentences = pendingSentences
                , selectedSentences = Array.empty
                , deselectedSentences =
                    pendingSentences
                        |> Array.indexedMap (\index _ -> index)
              }
            , Cmd.none
            , Action.none
            )

        ReceivedPendingSentencesList (Err _) ->
            Debug.todo "handle error"

        Selected index ->
            ( { model
                | selectedSentences =
                    Array.push index model.selectedSentences
                , deselectedSentences =
                    Array.filter
                        ((/=) index)
                        model.deselectedSentences
              }
            , Cmd.none
            , Action.none
            )

        Deselected index ->
            ( { model
                | selectedSentences =
                    Array.filter
                        ((/=) index)
                        model.selectedSentences
                , deselectedSentences =
                    Array.push index model.deselectedSentences
              }
            , Cmd.none
            , Action.none
            )

        ConfirmBatchClicked ->
            ( { model | confirmBatchRequesState = Loading }
            , Cmd.none
            , Action.uuid UuidReceived
            )

        UuidReceived uuid ->
            ( model
            , Cmd.none
            , confirmBatchRequest
                { pendingSentences = model.pendingSentences
                , selectedSentences = model.selectedSentences
                , deselectedSentences = model.deselectedSentences
                , batchId = uuid
                }
                |> Task.sheetsAttempt BatchConfirmed
                |> Action.google
            )

        BatchConfirmed (Ok _) ->
            ( { model
                | confirmBatchRequesState = Idle
                , pendingSentences = Array.empty
                , selectedSentences = Array.empty
                , deselectedSentences = Array.empty
              }
            , Cmd.none
            , Action.none
            )

        BatchConfirmed (Err _) ->
            ( { model
                | confirmBatchRequesState = Idle
              }
            , Cmd.none
            , Action.none
            )


getPendingSentencesRequest : Task.SheetsTask (Array Model.PendingSentence)
getPendingSentencesRequest =
    Sheets.getSubSheetDataRequest
        [ { sheetId = Constants.subSheetId PendingSentences
          , startRowIndex = Just 1
          , endRowIndex = Nothing
          , startColumnIndex = Just 0
          , endColumnIndex = Just 4
          }
        ]
        |> Task.map (Model.fromGridData maybeConstructPendingSentence)
        |> Task.map Array.fromList


maybeConstructPendingSentence :
    List Sheets.ResponseCellData
    -> Maybe Model.PendingSentence
maybeConstructPendingSentence =
    constructFromList Model.PendingSentence
        >> field cellStringValue
        >> field cellStringValue
        >> field
            (cellStringValue
                >> Maybe.map (Decode.decodeString (Decode.list Decode.string))
                >> Maybe.andThen Result.toMaybe
            )
        >> field
            (cellStringValue
                >> Maybe.map Iso8601.toTime
                >> Maybe.andThen Result.toMaybe
            )
        >> extract


confirmBatchRequest :
    { pendingSentences : Array Model.PendingSentence
    , selectedSentences : Array Int
    , deselectedSentences : Array Int
    , batchId : String
    }
    -> Task.SheetsTask ()
confirmBatchRequest config =
    let
        { pendingSentences, selectedSentences, deselectedSentences, batchId } =
            config

        selectedPendingSentences =
            sentencesFromIndexes
                selectedSentences
                pendingSentences

        deselectedPendingSentences =
            sentencesFromIndexes
                deselectedSentences
                pendingSentences
    in
    Task.platform Time.now
        |> Task.andThen
            (\time ->
                Sheets.batchUpdateRequest
                    [ RequestAppendCells
                        { sheetId = Constants.subSheetId MinedSentences
                        , rows =
                            selectedPendingSentences
                                |> List.map
                                    (\( _, { word, sentence, tags } ) ->
                                        [ RequestString word
                                        , RequestString sentence
                                        , Sheets.tagsExtendedValue tags
                                        , RequestString batchId
                                        , Sheets.iso8601ExtendedValue time
                                        ]
                                    )
                                |> Sheets.requestRows
                        , fields = "userEnteredValue"
                        }
                    , RequestAppendCells
                        { sheetId = Constants.subSheetId MinedWords
                        , rows =
                            selectedPendingSentences
                                |> List.map
                                    (\( _, { word } ) ->
                                        [ RequestString word
                                        , Sheets.iso8601ExtendedValue time
                                        ]
                                    )
                                |> Sheets.requestRows
                        , fields = "userEnteredValue"
                        }
                    , RequestAppendCells
                        { sheetId = Constants.subSheetId BacklogSentences
                        , rows =
                            deselectedPendingSentences
                                |> List.map
                                    (\( _, { word, sentence, tags } ) ->
                                        [ RequestString word
                                        , RequestString sentence
                                        , Sheets.tagsExtendedValue tags
                                        , Sheets.iso8601ExtendedValue time
                                        ]
                                    )
                                |> Sheets.requestRows
                        , fields = "userEnteredValue"
                        }
                    , RequestDeleteRange
                        { range =
                            { sheetId = Constants.subSheetId PendingSentences
                            , startRowIndex = Just 1
                            , endRowIndex = Nothing
                            , startColumnIndex = Just 0
                            , endColumnIndex = Just 4
                            }
                        , dimension = RequestRows
                        }
                    ]
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Pending Sentences"
    , content =
        div []
            [ ul []
                (viewSentences
                    model.selectedSentences
                    model.pendingSentences
                    Deselected
                    (if model.confirmBatchRequesState == Loading then
                        Disabled

                     else
                        Enabled
                    )
                )
            , hr [] []
            , ul []
                (viewSentences
                    model.deselectedSentences
                    model.pendingSentences
                    Selected
                    (if
                        isSelectionComplete model
                            || model.confirmBatchRequesState
                            == Loading
                     then
                        Disabled

                     else
                        Enabled
                    )
                )
            , hr [] []
            , button
                [ disabled <|
                    (not (isSelectionComplete model)
                        || model.confirmBatchRequesState
                        == Loading
                    )
                , onClick ConfirmBatchClicked
                ]
                [ text "Confirm batch" ]
            ]
    }


type Ableness
    = Disabled
    | Enabled


viewSentences :
    Array Int
    -> Array Model.PendingSentence
    -> (Int -> Msg)
    -> Ableness
    -> List (Html Msg)
viewSentences indexes pendingSentences onButtonClick ableness =
    sentencesFromIndexes indexes pendingSentences
        |> List.map
            (\( index, { word, sentence } ) ->
                li []
                    [ text (word ++ " - " ++ sentence)
                    , span [] [ text " " ]
                    , button
                        [ onClick <| onButtonClick index
                        , disabled (ableness == Disabled)
                        ]
                        [ text "<>" ]
                    ]
            )


sentencesFromIndexes :
    Array Int
    -> Array Model.PendingSentence
    -> List ( Int, Model.PendingSentence )
sentencesFromIndexes indexes pendingSentences =
    indexes
        |> Array.toList
        |> List.map
            (\index ->
                Array.get index pendingSentences
                    |> Maybe.map (\sentence -> ( index, sentence ))
            )
        |> List.filterMap identity


isSelectionComplete : Model -> Bool
isSelectionComplete model =
    let
        selectedSentencesLength =
            Array.length model.selectedSentences
    in
    selectedSentencesLength
        == pendingSentencesNumberToBatch
        || selectedSentencesLength
        == Array.length model.pendingSentences



-- CONSTANTS


pendingSentencesNumberToBatch : Int
pendingSentencesNumberToBatch =
    10
