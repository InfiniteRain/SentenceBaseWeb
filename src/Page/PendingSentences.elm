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
import Api.Google.ParamTask as ParamTask exposing (ParamTask)
import Api.Google.Requests as Requests
    exposing
        ( field
        , maybeConstruct
        , stringValue
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
    }


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
            ( model, Cmd.none, Action.uuid UuidReceived )

        UuidReceived uuid ->
            let
                _ =
                    Debug.log "uuid" uuid
            in
            ( model, Cmd.none, Action.none )


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
                        >> Array.fromList
                        >> maybeConstructPendingSentence
                    )
                >> List.filterMap identity
                >> Array.fromList
            )


maybeConstructPendingSentence :
    Array Requests.SheetResponseCellData
    -> Maybe PendingSentence
maybeConstructPendingSentence row =
    maybeConstruct PendingSentence
        |> field
            (row
                |> Array.get 0
                |> Maybe.andThen stringValue
            )
        |> field
            (row
                |> Array.get 1
                |> Maybe.andThen stringValue
            )
        |> field
            (row
                |> Array.get 2
                |> Maybe.andThen stringValue
                |> Maybe.map (Decode.decodeString (Decode.list Decode.string))
                |> Maybe.andThen Result.toMaybe
            )
        |> field
            (row
                |> Array.get 3
                |> Maybe.andThen stringValue
                |> Maybe.map Iso8601.toTime
                |> Maybe.andThen Result.toMaybe
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
                    Enabled
                )
            , hr [] []
            , ul []
                (viewSentences
                    model.deselectedSentences
                    model.pendingSentences
                    Selected
                    (if isSelectionComplete model then
                        Disabled

                     else
                        Enabled
                    )
                )
            , hr [] []
            , button
                [ disabled (not <| isSelectionComplete model)
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
    indexes
        |> Array.toList
        |> List.map
            (\index ->
                Array.get index pendingSentences
                    |> Maybe.map (\sentence -> ( index, sentence ))
            )
        |> List.filterMap identity
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
