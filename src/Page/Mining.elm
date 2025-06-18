module Page.Mining exposing (Model, Msg(..), init, subscriptions, update, view)

import Api.Action as Action exposing (Action(..))
import Api.Google.Constants as Constants exposing (SubSheet(..))
import Api.Google.Exchange.Sheets as Sheets
    exposing
        ( RequestBatchUpdateKind(..)
        , RequestDimension(..)
        , RequestExtendedValue(..)
        )
import Api.Google.Exchange.Task as Task
import Api.Google.ListConstructor exposing (cellStringValue, constructFromList, extract, field)
import Api.Wiktionary as Wiktionary exposing (Definitions(..), Usages(..))
import Html exposing (Attribute, Html, br, button, div, input, li, span, text, ul)
import Html.Attributes exposing (class, disabled, style, type_, value)
import Html.Events exposing (onClick, onInput, stopPropagationOn)
import Http
import Iso8601
import Json.Decode as Decode
import Port
import Regex
import RegexExtra
import Session exposing (Session)
import Task as PlatformTask
import TaskPort
import Time



-- MODEL


type alias Model =
    { session : Session
    , sentence : String
    , sentenceWords : List String
    , tagsInput : String
    , selectedWord : Maybe String
    , definitionState : DefinitionState
    , addRequestState : AddRequestState
    }


type DefinitionState
    = WordNotSelected
    | Loading
    | Fetched Usages
    | NotFound


type AddRequestState
    = Idle
    | Sending
    | Error


init : Session -> ( Model, Cmd Msg, Action Msg )
init session =
    ( { session = session
      , sentence = ""
      , sentenceWords = []
      , tagsInput = ""
      , selectedWord = Nothing
      , definitionState = WordNotSelected
      , addRequestState = Idle
      }
    , Cmd.none
    , Action.none
    )



-- UPDATE


type Msg
    = BodyClicked
    | ClipboardUpdated (TaskPort.Result String)
    | WordSelected String
    | DefinitionFetched (Result Http.Error Wiktionary.Usages)
    | MineClicked
    | GotAddPendingSentenceResponse (Result Task.Error ())
    | OnTagsInputChanged String
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg, Action Msg )
update msg model =
    case msg of
        ClipboardUpdated (Ok str) ->
            if str == model.sentence then
                ( model, Cmd.none, Action.none )

            else
                ( { model
                    | sentence = String.trim str
                    , sentenceWords = RegexExtra.sentenceSplit str
                    , selectedWord = Nothing
                    , definitionState = WordNotSelected
                  }
                , Cmd.none
                , Action.none
                )

        ClipboardUpdated (Err _) ->
            ( model, Cmd.none, Action.none )

        WordSelected str ->
            ( { model | selectedWord = Just str, definitionState = Loading }
            , Cmd.none
            , Action.wiktionary DefinitionFetched str
            )

        DefinitionFetched result ->
            case ( result, model.selectedWord ) of
                ( Ok definition, Just _ ) ->
                    ( { model | definitionState = Fetched definition }
                    , Cmd.none
                    , Action.none
                    )

                ( Err _, Just _ ) ->
                    ( { model | definitionState = NotFound }
                    , Cmd.none
                    , Action.none
                    )

                _ ->
                    ( model, Cmd.none, Action.none )

        BodyClicked ->
            ( model
            , PlatformTask.attempt ClipboardUpdated Port.readClipboard
            , Action.none
            )

        MineClicked ->
            ( { model
                | sentence = ""
                , sentenceWords = []
                , selectedWord = Nothing
                , definitionState = WordNotSelected
              }
            , Cmd.none
            , addPendingSentenceRequest
                { word = model.selectedWord |> Maybe.withDefault ""
                , sentence = model.sentence
                , tags =
                    model.tagsInput
                        |> String.trim
                        |> Regex.split RegexExtra.space
                        |> List.filter ((/=) "")
                }
                |> Task.sheetsAttempt GotAddPendingSentenceResponse
                |> Action.google
            )

        GotAddPendingSentenceResponse resp ->
            let
                _ =
                    Debug.log "words: " resp
            in
            ( model, Cmd.none, Action.none )

        OnTagsInputChanged text ->
            ( { model | tagsInput = text }
            , Cmd.none
            , Action.none
            )

        Noop ->
            ( model, Cmd.none, Action.none )


type alias PendingSentence =
    { word : String
    , sentence : String
    , tags : List String
    }


type alias MinedWord =
    { word : String
    , mined_at : Time.Posix
    }


addPendingSentenceRequest :
    PendingSentence
    -> Task.SheetsTask ()
addPendingSentenceRequest { word, sentence, tags } =
    Task.platform Time.now
        |> Task.andThen
            (\time ->
                Sheets.batchUpdateAndGetGridDataRequest
                    [ AppendCells
                        { sheetId = Constants.subSheetId PendingSentences
                        , rows =
                            Sheets.sheetRequestRow
                                [ StringValue word
                                , StringValue sentence
                                , Sheets.tagsExtendedValue tags
                                , Sheets.iso8601ExtendedValue time
                                ]
                        , fields = "userEnteredValue"
                        }
                    , UpdateCells
                        { rows =
                            Sheets.sheetRequestRow
                                [ FormulaValue
                                    ("=QUERY("
                                        ++ Constants.subSheetName MinedWords
                                        ++ "!"
                                        ++ "A2:B,"
                                        ++ "\""
                                        ++ "SELECT * WHERE A != \"\""
                                        ++ String.replace "\"" "" word
                                        ++ "\"\""
                                        ++ "\""
                                        ++ ")"
                                    )
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
                    [ Constants.subSheetName Query ++ "!A1:B" ]
            )
        |> Task.map extractMinedWords
        |> Task.andThen
            (\mined_words ->
                -- a fail safe for a case where the query fails for whatever
                -- reason, so that it doesn't simply nuke the entire subsheet
                --
                -- this has a side effect of preventing a valid case that
                -- would leave the mined_words spreadsheet empty (when the user
                -- mines a word that happens to be the only word in mined_words
                -- spreadsheet)
                if List.length mined_words == 0 then
                    Task.succeed ()

                else
                    -- potential race condition can happen here if the same user
                    -- tries to mine a word on two devices at the same time
                    Sheets.batchUpdateRequest
                        [ UpdateCells
                            { rows =
                                Sheets.sheetRequestRows <|
                                    List.map
                                        (\mined_word ->
                                            [ StringValue mined_word.word
                                            , StringValue <|
                                                Iso8601.fromTime
                                                    mined_word.mined_at
                                            ]
                                        )
                                        (Debug.log "mined_words" mined_words)
                            , fields = "userEnteredValue"
                            , range =
                                { sheetId = Constants.subSheetId MinedWords
                                , startRowIndex = Just 1
                                , endRowIndex = Nothing
                                , startColumnIndex = Just 0
                                , endColumnIndex = Just 2
                                }
                            }
                        ]
            )


extractMinedWords : Sheets.ResponseBatchUpdate -> List MinedWord
extractMinedWords batchUpdate =
    batchUpdate.updatedSpreadsheet.sheets
        |> List.head
        |> Maybe.map .data
        |> Maybe.andThen List.head
        |> Maybe.andThen .rowData
        |> Maybe.withDefault []
        |> List.map
            (.values
                >> Maybe.withDefault []
                >> constructFromList MinedWord
                >> field cellStringValue
                >> field
                    (cellStringValue
                        >> Maybe.map Iso8601.toTime
                        >> Maybe.andThen Result.toMaybe
                    )
                >> extract
            )
        |> List.filterMap identity



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Mining"
    , content =
        div
            [ style "width" "100%"
            , style "height" "100%"
            , onClick BodyClicked
            ]
            (List.concat
                [ List.map
                    (\word ->
                        button
                            [ onClick (WordSelected (String.toLower word)) ]
                            [ text word ]
                    )
                    model.sentenceWords
                , model.selectedWord
                    |> Maybe.map
                        (\word ->
                            [ br [] []
                            , span [] [ text word ]
                            ]
                        )
                    |> Maybe.withDefault []
                , [ br [] [] ]
                , case model.definitionState of
                    WordNotSelected ->
                        [ span [] [ text "Select a word" ] ]

                    Loading ->
                        [ span [] [ text "Fetching definitions..." ] ]

                    Fetched definition ->
                        [ usagesView definition ]

                    NotFound ->
                        [ span [] [ text "Definition was not found" ] ]
                , [ br [] [] ]
                , [ input
                        [ type_ "text"
                        , value model.tagsInput
                        , onClick Noop
                        , onInput OnTagsInputChanged
                        ]
                        []
                  ]
                , [ br [] [] ]
                , [ button
                        [ disabled (model.selectedWord == Nothing)
                        , onClick MineClicked
                        ]
                        [ text "Mine" ]
                  ]
                , [ br [] [] ]
                , [ br [] [] ]
                , [ br [] [] ]
                , [ br [] [] ]
                , [ br [] [] ]
                , [ div [] [ text "wij hebben een serieus probleem" ] ]
                , [ div [] [ text "geef me een klap papa" ] ]
                , [ div [] [ text "kan je 't goed zien?" ] ]
                ]
            )
    }


usagesView : Wiktionary.Usages -> Html Msg
usagesView (Usages usages) =
    div [] (List.indexedMap definitionsView usages)


definitionsView : Int -> Wiktionary.Definitions -> Html Msg
definitionsView index (Definitions definitions) =
    div [ class "etimology" ]
        [ text <| "Etimology " ++ String.fromInt (index + 1)
        , ul []
            (List.map definitionView definitions)
        ]


definitionView : Wiktionary.Definition -> Html Msg
definitionView definition =
    li [] <|
        List.concat
            [ Regex.split RegexExtra.newLines definition.text
                |> List.map (\line -> div [] [ text (String.trim line) ])
            , definition.formUsages |> List.concatMap formUsagesView
            ]


formUsagesView : Wiktionary.FormUsages -> List (Html Msg)
formUsagesView { word, usages } =
    let
        (Usages formUsages) =
            usages
    in
    List.indexedMap
        (\index (Definitions definitions) ->
            div []
                [ div [ class "etimology" ]
                    [ text <|
                        "Etimology "
                            ++ String.fromInt (index + 1)
                            ++ " for "
                            ++ word
                    , ul [] <|
                        List.map
                            (\definition ->
                                li []
                                    (Regex.split
                                        RegexExtra.newLines
                                        definition.text
                                        |> List.map
                                            (\line ->
                                                div
                                                    []
                                                    [ text (String.trim line) ]
                                            )
                                    )
                            )
                            definitions
                    ]
                ]
        )
        formUsages


onClick : msg -> Attribute msg
onClick msg =
    stopPropagationOn "click" <| Decode.succeed ( msg, True )
