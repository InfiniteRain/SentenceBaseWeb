module Page.Batches exposing (Model, Msg(..), init, subscriptions, update, view)

import Api.Google.Constants as Constants exposing (SubSheet(..))
import Api.Google.Exchange.Sheets as Sheets
    exposing
        ( RequestBatchUpdateKind(..)
        , RequestDimension(..)
        , RequestExtendedValue(..)
        , requestRow
        )
import Api.Google.Exchange.Task as Task
import Api.Google.ListConstructor
    exposing
        ( cellNumberValue
        , cellStringValue
        , constructFromList
        , extractWithRest
        , field
        )
import Api.Google.Model as Model
import Basecoat exposing (ariaInvalid, ariaLabel, classes, modalDialog)
import Effect exposing (Effect)
import Html exposing (Html, article, button, div, footer, form, h2, header, input, label, p, section, text)
import Html.Attributes exposing (class, disabled, for, id, type_, value)
import Html.Events exposing (onClick, onInput)
import Icon.Cross exposing (crossIcon)
import Icon.Info exposing (infoIcon)
import Icon.Loading exposing (loadingIcon)
import Icon.WarningCircle exposing (warningCircleIcon)
import Json.Decode as Decode
import Json.Encode as Encode
import Port.Anki as Anki
import Port.LocalStorage as LocalStorage
import Session exposing (Session)
import Task as PlatformTask
import TaskPort
import Time exposing (Month(..))



-- MODEL


type alias Model =
    { session : Session
    , currentPage : Int
    , currentBatches : List (List Model.MinedSentence)
    , reachedEnd : Bool
    , getState : GetState
    , exportForm : ExportForm
    , exportState : ExportState
    }


type GetState
    = GetLoading
    | GetSuccess
    | GetFail Task.Error


type alias ExportForm =
    { isOpen : Bool
    , batch : List Model.MinedSentence
    , key : String
    , region : String
    , validation : ExportFormValidation
    }


type alias ExportFormValidation =
    { key : Maybe String
    , region : Maybe String
    }


type alias SpeechConfig =
    { key : String
    , region : String
    }


init : Session -> ( Model, Cmd Msg, Effect Msg )
init session =
    ( { session = session
      , currentPage = 0
      , currentBatches = []
      , reachedEnd = False
      , getState = GetLoading
      , exportForm =
            { isOpen = False
            , batch = []
            , key = ""
            , region = ""
            , validation =
                { key = Just ""
                , region = Just ""
                }
            }
      , exportState = ExportIdle
      }
    , LocalStorage.get
        (Decode.map2 SpeechConfig
            (Decode.field "key" Decode.string)
            (Decode.field "region" Decode.string)
        )
        speechConfigLocalStorageKey
        |> PlatformTask.attempt SpeechConfigLocalStorageFetched
    , getBatchesEffect 0
    )



-- UPDATE


type Msg
    = SpeechConfigLocalStorageFetched (Result Decode.Error SpeechConfig)
    | GetFetched (Result Task.Error (List (List Model.MinedSentence)))
    | LoadMoreClicked
    | BatchClicked (List Model.MinedSentence)
    | ExportDialogClosed
    | ExportFormUpdated ExportFormInput
    | ExportConfirmClicked
    | UuidsReceived (List String)
    | NoOp


type ExportFormInput
    = ExportFormKey String
    | ExportFormRegion String


type ExportState
    = ExportIdle
    | ExportLoading
    | ExportFinished ()


update : Msg -> Model -> ( Model, Cmd Msg, Effect Msg )
update msg ({ exportForm } as model) =
    case msg of
        SpeechConfigLocalStorageFetched (Ok config) ->
            ( { model
                | exportForm = setExportForm config.key config.region exportForm
              }
            , Cmd.none
            , Effect.none
            )

        SpeechConfigLocalStorageFetched (Err _) ->
            ( { model
                | exportForm = setExportForm "" "" exportForm
              }
            , LocalStorage.remove speechConfigLocalStorageKey
                |> PlatformTask.perform (\_ -> NoOp)
            , Effect.none
            )

        GetFetched (Ok batches) ->
            ( { model
                | getState = GetSuccess
                , currentPage = model.currentPage + 1
                , currentBatches = model.currentBatches ++ batches
                , reachedEnd = List.length batches < pageSize
              }
            , Cmd.none
            , Effect.none
            )

        GetFetched (Err err) ->
            ( { model | getState = GetFail err }
            , Cmd.none
            , Effect.none
            )

        LoadMoreClicked ->
            ( { model | getState = GetLoading }
            , Cmd.none
            , getBatchesEffect model.currentPage
            )

        BatchClicked batch ->
            ( { model
                | exportForm =
                    { exportForm
                        | isOpen = True
                        , batch = batch
                    }
              }
            , Cmd.none
            , Effect.none
            )

        ExportDialogClosed ->
            ( { model | exportForm = { exportForm | isOpen = False } }
            , Cmd.none
            , Effect.none
            )

        ExportFormUpdated input ->
            let
                newForm =
                    case input of
                        ExportFormKey key ->
                            setExportForm key exportForm.region exportForm

                        ExportFormRegion region ->
                            setExportForm exportForm.key region exportForm
            in
            ( { model | exportForm = newForm }
            , LocalStorage.set
                (encodeForm newForm)
                speechConfigLocalStorageKey
                |> PlatformTask.perform (\_ -> NoOp)
            , Effect.none
            )

        ExportConfirmClicked ->
            ( model
            , Cmd.none
            , Effect.uuids
                (List.length model.exportForm.batch)
                UuidsReceived
            )

        UuidsReceived uuids ->
            ( model
            , PlatformTask.sequence
                (List.map
                    (\{ sentence } ->
                        generateAudio
                            { key = model.exportForm.key
                            , region = model.exportForm.region
                            , sentence = sentence
                            }
                    )
                    model.exportForm.batch
                )
                |> PlatformTask.map
                    (List.map3
                        (\fileName sentenceDatum fileData ->
                            ( fileName ++ ".mp3"
                            , sentenceDatum
                            , fileData
                            )
                        )
                        uuids
                        model.exportForm.batch
                    )
                |> PlatformTask.andThen
                    (\sentenceData ->
                        ankiDeck
                            |> Anki.addFiles
                                (List.map
                                    (\( fileName, _, fileData ) ->
                                        ( fileName, fileData )
                                    )
                                    sentenceData
                                )
                            |> Anki.addNotes
                                (List.map
                                    (\( fileName, { word, sentence }, _ ) ->
                                        [ sentence
                                        , ""
                                        , word
                                        , ""
                                        , "[sound:" ++ fileName ++ "]"
                                        ]
                                    )
                                    sentenceData
                                )
                                ankiModel
                            |> Anki.export "export.apkg"
                    )
                |> PlatformTask.attempt (\_ -> NoOp)
            , Effect.none
            )

        NoOp ->
            ( model, Cmd.none, Effect.none )


encodeForm : ExportForm -> Encode.Value
encodeForm { key, region } =
    Encode.object
        [ ( "key", Encode.string key )
        , ( "region", Encode.string region )
        ]


setExportForm : String -> String -> ExportForm -> ExportForm
setExportForm key region ({ validation } as form) =
    { form
        | key = key
        , region = region
        , validation =
            { validation
                | key = nonEmpty key "Key field cannot be empty"
                , region = nonEmpty region "Region field cannot be empty"
            }
    }


nonEmpty : String -> String -> Maybe String
nonEmpty value errorString =
    case value of
        "" ->
            Just errorString

        _ ->
            Nothing


getBatchesRequest : Int -> Task.SheetsTask (List (List Model.MinedSentence))
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
                            ++ "SELECT COUNT(D), D, AVG(E) "
                            ++ "WHERE D <> '' "
                            ++ "GROUP BY D "
                            ++ "ORDER BY AVG(E) DESC "
                            ++ "LIMIT "
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
        |> Task.andThen
            (\ids ->
                Sheets.batchUpdateAndGetGridDataRequest
                    (RequestDeleteRange
                        { range =
                            { sheetId = Constants.subSheetId Query
                            , startRowIndex = Just 0
                            , endRowIndex = Nothing
                            , startColumnIndex = Just 0
                            , endColumnIndex = Nothing
                            }
                        , dimension = RequestRows
                        }
                        :: List.indexedMap
                            (\index id ->
                                RequestUpdateCells
                                    { rows =
                                        requestRow
                                            [ RequestFormula <|
                                                "=QUERY("
                                                    ++ Constants.subSheetName
                                                        MinedSentences
                                                    ++ "!A1:E,\""
                                                    ++ "SELECT * WHERE D = '"
                                                    ++ id
                                                    ++ "' OR D = 'batch_id'"
                                                    ++ "\")"
                                            ]
                                    , fields = "userEnteredValue"
                                    , range =
                                        { sheetId = Constants.subSheetId Query
                                        , startRowIndex = Just 0
                                        , endRowIndex = Nothing
                                        , startColumnIndex = Just (index * 5)
                                        , endColumnIndex =
                                            Just
                                                ((index * 5) + 5)
                                        }
                                    }
                            )
                            ids
                    )
                    [ Constants.subSheetName Query ++ "!A2:ZZ" ]
            )
        |> Task.map
            (.updatedSpreadsheet
                >> Model.fromGridDataMatrix maybeConstructMinedSentence
            )


constructBatchIds : List Sheets.ResponseCellData -> Maybe String
constructBatchIds =
    List.head >> Maybe.andThen cellStringValue


maybeConstructMinedSentence :
    List Sheets.ResponseCellData
    -> ( Maybe Model.MinedSentence, List Sheets.ResponseCellData )
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
            (cellNumberValue
                >> Maybe.map round
                >> Maybe.map Time.millisToPosix
            )
        >> extractWithRest


getBatchesEffect : Int -> Effect Msg
getBatchesEffect page =
    getBatchesRequest page
        |> Task.sheetsAttempt GetFetched
        |> Effect.google



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Batches"
    , content =
        div
            [ classes
                [ "flex"
                , "shrink-0"
                , "grow-0"
                , "justify-center"
                , "h-full"
                ]
            ]
            [ batchesView model ]
    }


batchesView : Model -> Html Msg
batchesView model =
    div [ classes [ "flex", "flex-col", "basis-md", "h-full" ] ]
        [ div
            [ classes
                [ "grow-1"
                , "shrink-0"
                , "overflow-auto"
                , "basis-0"
                ]
            ]
          <|
            List.concat
                [ List.map (batchView model) model.currentBatches
                , case ( model.getState, model.currentBatches ) of
                    ( GetLoading, _ ) ->
                        [ batchSkeletonView [ "w-[95%]", "w-[30%]" ]
                        , batchSkeletonView [ "w-[85%]" ]
                        , batchSkeletonView [ "w-[97%]", "w-[50%]" ]
                        ]

                    ( GetSuccess, [] ) ->
                        [ div [ class "alert" ]
                            [ infoIcon []
                            , h2 [] [ text "No batches have yet been made." ]
                            ]
                        ]

                    ( GetSuccess, _ ) ->
                        []

                    ( GetFail err, _ ) ->
                        [ div
                            [ classes
                                [ "alert-destructive"
                                , "[&:not(:first-child)]:mt-4"
                                ]
                            ]
                            [ warningCircleIcon []
                            , h2 [] [ text "Unable to fetch batches." ]
                            , section [] [ text <| Task.errorToMessage err ]
                            ]
                        ]
                , if
                    model.currentPage
                        == 1
                        && List.isEmpty model.currentBatches
                  then
                    []

                  else
                    [ button
                        [ classes [ "btn-ghost", "w-full", "mt-4" ]
                        , disabled
                            (model.getState
                                == GetLoading
                                || model.reachedEnd
                            )
                        , onClick LoadMoreClicked
                        ]
                        [ text <|
                            if model.reachedEnd then
                                "No more batches to load"

                            else
                                "Load more"
                        ]
                    ]
                ]
        , exportDialogView model
        ]


exportDialogView : Model -> Html Msg
exportDialogView model =
    modalDialog
        model.exportForm.isOpen
        ExportDialogClosed
        [ classes
            [ "dialog"
            , "w-full"
            , "sm:max-w-[425px]"
            , "max-h-[612px]"
            ]
        ]
        [ article []
            [ header []
                [ h2 [] [ text "Export batch" ] ]
            , section []
                [ form [ classes [ "form", "grid", "gap-4" ] ]
                    [ div [ classes [ "grid", "gap-3" ] ] <|
                        List.concat
                            [ [ label [ for "export-speech-key" ]
                                    [ text "Azure Speech Key" ]
                              , input
                                    [ type_ "text"
                                    , id "export-speech-key"
                                    , value model.exportForm.key
                                    , onInput
                                        (ExportFormUpdated << ExportFormKey)
                                    , ariaInvalid
                                        (model.exportForm.validation.key
                                            /= Nothing
                                        )
                                    ]
                                    []
                              ]
                            , case model.exportForm.validation.key of
                                Just error ->
                                    [ p
                                        [ classes
                                            [ "text-destructive"
                                            , "text-sm"
                                            ]
                                        ]
                                        [ text error ]
                                    ]

                                _ ->
                                    []
                            ]
                    , div [ classes [ "grid", "gap-3" ] ] <|
                        List.concat
                            [ [ label [ for "export-speech-region" ]
                                    [ text "Azure Speech Region" ]
                              , input
                                    [ type_ "text"
                                    , id "export-speech-region"
                                    , value model.exportForm.region
                                    , onInput
                                        (ExportFormUpdated << ExportFormRegion)
                                    , ariaInvalid
                                        (model.exportForm.validation.region
                                            /= Nothing
                                        )
                                    ]
                                    []
                              ]
                            , case model.exportForm.validation.region of
                                Just error ->
                                    [ p
                                        [ classes
                                            [ "text-destructive"
                                            , "text-sm"
                                            ]
                                        ]
                                        [ text error ]
                                    ]

                                _ ->
                                    []
                            ]
                    ]
                ]
            , footer []
                [ button
                    [ class "btn-outline"
                    , onClick ExportDialogClosed
                    , disabled (model.exportState == ExportLoading)
                    ]
                    [ text "Cancel" ]
                , button
                    [ class "btn"
                    , onClick ExportConfirmClicked
                    , disabled
                        (model.exportState
                            == ExportLoading
                            || model.exportForm.validation.key
                            /= Nothing
                            || model.exportForm.validation.region
                            /= Nothing
                        )
                    ]
                  <|
                    List.concat
                        [ case model.exportState of
                            ExportLoading ->
                                [ loadingIcon [] ]

                            _ ->
                                []
                        , [ text "Export batch" ]
                        ]
                ]
            , button
                [ type_ "button"
                , ariaLabel "Close dialog"
                , onClick ExportDialogClosed
                ]
                [ crossIcon [] ]
            ]
        ]


batchSkeletonView : List String -> Html Msg
batchSkeletonView wordsSizes =
    div
        [ classes
            [ "flex"
            , "flex-col"
            , "items-start"
            , "gap-3"
            , "border"
            , "p-3"
            , "[&:not(:first-child)]:mt-4"
            ]
        ]
    <|
        List.map
            (\wordsSize ->
                div
                    [ classes
                        [ "bg-accent"
                        , "animate-pulse"
                        , "rounded-md"
                        , "h-4"
                        , wordsSize
                        ]
                    ]
                    []
            )
            wordsSizes


batchView : Model -> List Model.MinedSentence -> Html Msg
batchView model batch =
    let
        batchedAt =
            List.head batch
                |> Maybe.map .minedAt
                |> Maybe.withDefault (Time.millisToPosix 0)
    in
    label
        [ classes
            [ "flex"
            , "items-start"
            , "gap-3"
            , "border"
            , "p-3"
            , "hover:bg-accent/50"
            , "rounded-lg"
            , "has-[button:disabled]:opacity-50"
            , "[&:not(:first-child)]:mt-4"
            ]
        ]
        [ button
            [ type_ "button"
            , onClick (BatchClicked batch)
            ]
            []
        , div [ classes [ "grid", "gap-2" ] ] <|
            [ h2
                [ classes
                    [ "text-sm"
                    , "leading-none"
                    , "font-medium"
                    ]
                ]
                [ text
                    ("Mined on "
                        ++ posixToString
                            (Session.zone model.session)
                            batchedAt
                    )
                ]
            , p
                [ class "text-sm" ]
                [ text
                    (batch
                        |> List.map .word
                        |> String.join ", "
                    )
                ]
            ]
        ]


posixToString : Time.Zone -> Time.Posix -> String
posixToString zone posix =
    (Time.toDay zone posix |> String.fromInt)
        ++ " "
        ++ (Time.toMonth zone posix |> monthToString)
        ++ " "
        ++ (Time.toYear zone posix |> String.fromInt)
        ++ ", "
        ++ (Time.toHour zone posix |> String.fromInt |> String.padLeft 2 '0')
        ++ ":"
        ++ (Time.toMinute zone posix |> String.fromInt |> String.padLeft 2 '0')


monthToString : Time.Month -> String
monthToString month =
    case month of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"



-- PORT CALLS


generateAudio :
    { key : String
    , region : String
    , sentence : String
    }
    -> TaskPort.Task String
generateAudio =
    TaskPort.call
        { function = "generateAudio"
        , valueDecoder = Decode.string
        , argsEncoder =
            \{ key, region, sentence } ->
                Encode.object
                    [ ( "key", Encode.string key )
                    , ( "region", Encode.string region )
                    , ( "sentence", Encode.string sentence )
                    ]
        }



-- CONSTANTS


pageSize : Int
pageSize =
    10


ankiModel : Anki.Model
ankiModel =
    { id = 1755984462852
    , name = "Sentence Base Model"
    , fields =
        [ { name = "Sentence" }
        , { name = "Sentence Translation" }
        , { name = "Word" }
        , { name = "Word Definition" }
        , { name = "Sentence Audio" }
        ]
    , templates =
        [ { name = Nothing
          , frontHtml = String.trim """
<div class="body front-notice">Audio Only</div>

<div class="footer">{{Sentence Audio}}</div>
"""
          , backHtml = String.trim """
<div class="body">
  <div class="sentence">
    <div class="bigger">{{Sentence}}</div>
    <div class="bigger">{{Sentence Translation}}</div>
  </div>
  <hr />
  <div class="bigger">{{Word}}</div>
  <div class="definition">{{Word Definition}}</div>
</div>

<div class="footer">{{Sentence Audio}}</div>
"""
          }
        ]
    , styling = String.trim """
html,
body,
body>div {
  padding: 0;
  margin: 0;
  height: 100%;
}

.body {
  overflow: auto;
  text-align: center;
  height: calc(100% - 64px);
}

.footer {
  position: absolute;
  bottom: 0;
  height: 48px;
  width: 100%;
  display: flex;
  align-items: center;
  justify-content: center;
}

.front-notice {
  display: flex;
  align-items: center;
  justify-content: center;
  font-style: italic;
  color: #7e7f7a;
}

.bigger {
  font-size: 32px;
}

.sentence div {
  margin: 16px;
}

.definition {
  text-align: start;
  padding: 16px 48px 0 64px;
}

.definition ul {
  list-style: square;
  margin: 8px 0 0 0;
}

.definition ul:last-child {
  margin-bottom: 8px;
}

.definition li {
  margin-bottom: 4px;
}

.definition .etimology {
  font-size: 18px;
  margin-top: 8px;
}

.definition {
  font-size: 16px;
}
"""
    }


ankiDeck : Anki.Deck
ankiDeck =
    Anki.deck 1755984963683 "Sentence Base"


speechConfigLocalStorageKey : String
speechConfigLocalStorageKey =
    "speech_config"
