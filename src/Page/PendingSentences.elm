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
import Api.Google.ListConstructor as ListConstructor exposing (ListConstructor, cellStringValue, constructFromList, extract, field)
import Api.Google.ParamTask as ParamTask exposing (ParamTask)
import Api.Google.Requests as Requests
    exposing
        ( SheetRequestBatchUpdateKind(..)
        , SheetRequestDimension(..)
        , SheetRequestExtendedValue(..)
        )
import Array exposing (Array)
import Html exposing (Html, button, div, hr, li, span, text, ul)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)
import Iso8601
import Json.Decode as Decode
import Session exposing (Session)
import Task
import Time



-- MODEL


type alias Model =
    { session : Session
    , pendingSentences : Array PendingSentence
    , selectedSentences : Array Int
    , deselectedSentences : Array Int
    , confirmBatchRequesState : ConfirmBatchRequesState
    }


type ConfirmBatchRequesState
    = Idle
    | Loading


type alias PendingSentence =
    { word : String
    , sentence : String
    , tags : List String
    , added_at : Time.Posix
    }


init : Session -> ( Model, Cmd Msg, Action Msg )
init session =
    ( { session = session
      , pendingSentences = Array.empty
      , selectedSentences = Array.empty
      , deselectedSentences = Array.empty
      , confirmBatchRequesState = Idle
      }
    , Cmd.none
    , ParamTask.attempt ReceivedPendingSentencesList getPendingSentencesRequest
        |> Action.google
    )



-- UPDATE


type Msg
    = ReceivedPendingSentencesList
        (Result
            Requests.Error
            (Array PendingSentence)
        )
    | Selected Int
    | Deselected Int
    | ConfirmBatchClicked
    | UuidReceived String
    | BatchConfirmed (Result Requests.Error ())


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
                |> ParamTask.attempt BatchConfirmed
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


getPendingSentencesRequest : ParamTask Requests.Error (Array PendingSentence)
getPendingSentencesRequest =
    Requests.getSubSheetDataRequest
        [ { sheetId = Constants.subSheetId PendingSentences
          , startRowIndex = Just 1
          , endRowIndex = Nothing
          , startColumnIndex = Just 0
          , endColumnIndex = Just 4
          }
        ]
        >> Requests.buildTask
        >> Task.map
            (.sheets
                >> List.head
                >> Maybe.map .data
                >> Maybe.andThen List.head
                >> Maybe.andThen .rowData
                >> Maybe.withDefault []
                >> List.map
                    (.values
                        >> Maybe.withDefault []
                        >> maybeConstructPendingSentence
                    )
                >> List.filterMap identity
                >> Array.fromList
            )


maybeConstructPendingSentence :
    List Requests.SheetResponseCellData
    -> Maybe PendingSentence
maybeConstructPendingSentence row =
    constructFromList PendingSentence row
        |> field cellStringValue
        |> field cellStringValue
        |> field
            (cellStringValue
                >> Maybe.map (Decode.decodeString (Decode.list Decode.string))
                >> Maybe.andThen Result.toMaybe
            )
        |> field
            (cellStringValue
                >> Maybe.map Iso8601.toTime
                >> Maybe.andThen Result.toMaybe
            )
        |> extract


confirmBatchRequest :
    { pendingSentences : Array PendingSentence
    , selectedSentences : Array Int
    , deselectedSentences : Array Int
    , batchId : String
    }
    -> ParamTask Requests.Error ()
confirmBatchRequest config sheetId =
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
    Time.now
        |> Task.andThen
            (\time ->
                Requests.sheetBatchUpdateRequest
                    [ AppendCells
                        { sheetId = Constants.subSheetId MinedSentences
                        , rows =
                            selectedPendingSentences
                                |> List.map
                                    (\( _, { word, sentence, tags } ) ->
                                        [ StringValue word
                                        , StringValue sentence
                                        , Requests.tagsExtendedValue tags
                                        , StringValue batchId
                                        , Requests.iso8601ExtendedValue time
                                        ]
                                    )
                                |> Requests.sheetRequestRows
                        , fields = "userEnteredValue"
                        }
                    , AppendCells
                        { sheetId = Constants.subSheetId MinedWords
                        , rows =
                            selectedPendingSentences
                                |> List.map
                                    (\( _, { word } ) ->
                                        [ StringValue word
                                        , Requests.iso8601ExtendedValue time
                                        ]
                                    )
                                |> Requests.sheetRequestRows
                        , fields = "userEnteredValue"
                        }
                    , AppendCells
                        { sheetId = Constants.subSheetId BacklogSentences
                        , rows =
                            deselectedPendingSentences
                                |> List.map
                                    (\( _, { word, sentence, tags } ) ->
                                        [ StringValue word
                                        , StringValue sentence
                                        , Requests.tagsExtendedValue tags
                                        , Requests.iso8601ExtendedValue time
                                        ]
                                    )
                                |> Requests.sheetRequestRows
                        , fields = "userEnteredValue"
                        }
                    , DeleteRange
                        { range =
                            { sheetId = Constants.subSheetId PendingSentences
                            , startRowIndex = Just 1
                            , endRowIndex = Nothing
                            , startColumnIndex = Just 0
                            , endColumnIndex = Just 4
                            }
                        , dimension = Rows
                        }
                    ]
                    sheetId
                    |> Requests.buildTask
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
    -> Array PendingSentence
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
    -> Array PendingSentence
    -> List ( Int, PendingSentence )
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
