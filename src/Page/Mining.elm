module Page.Mining exposing (Model, Msg(..), init, subscriptions, update, view)

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
        , extract
        , field
        )
import Api.Google.Model as Model exposing (PendingSentence)
import Api.Wiktionary as Wiktionary exposing (Definitions(..), Usages(..))
import Array
import Basecoat
    exposing
        ( ariaControls
        , ariaLabel
        , ariaLabelledBy
        , ariaOrientation
        , ariaSelected
        , classes
        , modalDialog
        , role
        , tabIndex
        )
import Effect exposing (Effect(..))
import Html
    exposing
        ( Attribute
        , Html
        , article
        , button
        , div
        , footer
        , form
        , h2
        , header
        , input
        , label
        , li
        , nav
        , p
        , section
        , text
        , textarea
        , ul
        )
import Html.Attributes
    exposing
        ( checked
        , class
        , disabled
        , for
        , hidden
        , id
        , placeholder
        , style
        , type_
        , value
        )
import Html.Events exposing (onCheck, onInput, stopPropagationOn)
import Http
import Icon.Cross exposing (crossIcon)
import Icon.Info exposing (infoIcon)
import Icon.Loading exposing (loadingIcon)
import Icon.Plus exposing (plusIcon)
import Icon.SquarePen exposing (squarePenIcon)
import Icon.Trash exposing (trashIcon)
import Icon.WarningCircle exposing (warningCircleIcon)
import Json.Decode as Decode
import Port
import Regex
import RegexExtra
import Session exposing (Session)
import Set exposing (Set)
import Task as PlatformTask
import TaskPort
import Time
import Toast



-- MODEL


type alias Model =
    { session : Session
    , tab : Tab
    , sentence : String
    , sentenceWords : List String
    , selectedWord : Maybe String
    , definitionState : DefinitionState
    , addSentenceState : AddSentenceState
    , editSentenceState : EditSentenceState
    , getSentencesState : GetSentencesState
    , selectedSentences : Set Int
    , confirmBatchState : ConfirmBatchState
    , editSentenceForm : EditSentenceForm
    , deleteSentenceForm : DeleteSentenceForm
    }


type Tab
    = MiningTab
    | PendingSentencesTab


type DefinitionState
    = DefinitionWordNotSelected
    | DefinitionLoading
    | DefinitionFetched Usages
    | DefinitionNotFound


type AddSentenceState
    = AddSentenceIdle
    | AddSentenceLoading


type EditSentenceState
    = EditSentenceIdle
    | EditSentenceLoading


type GetSentencesState
    = GetSentencesStale
    | GetSentencesLoading
    | GetSentencesFetched (Result Task.Error (List PendingSentence))


type ConfirmBatchState
    = ConfirmBatchIdle
    | ConfirmBatchLoading


type alias EditSentenceForm =
    { isOpen : Bool
    , index : Int
    , sentence : PendingSentence
    , newTag : String
    }


type alias DeleteSentenceForm =
    { isOpen : Bool
    , index : Int
    , sentence : PendingSentence
    }


init : Session -> ( Model, Cmd Msg, Effect Msg )
init session =
    ( { session = session
      , tab = MiningTab
      , sentence = ""
      , sentenceWords = []
      , selectedWord = Nothing
      , definitionState = DefinitionWordNotSelected
      , addSentenceState = AddSentenceIdle
      , editSentenceState = EditSentenceIdle
      , getSentencesState = GetSentencesStale
      , selectedSentences = Set.empty
      , confirmBatchState = ConfirmBatchIdle
      , editSentenceForm =
            { isOpen = False
            , index = 0
            , sentence =
                { word = ""
                , sentence = ""
                , tags = []
                , addedAt = Time.millisToPosix 0
                }
            , newTag = ""
            }
      , deleteSentenceForm =
            { isOpen = False
            , index = 0
            , sentence =
                { word = ""
                , sentence = ""
                , tags = []
                , addedAt = Time.millisToPosix 0
                }
            }
      }
    , Cmd.none
    , Effect.none
    )



-- UPDATE


type Msg
    = TabClicked Tab
    | BodyClicked
    | ClipboardUpdated (TaskPort.Result String)
    | WordSelected String
    | DefinitionFetchedResponse (Result Http.Error Wiktionary.Usages)
    | MineClicked
    | GotAddPendingSentenceResponse (Result Task.Error ())
    | GotSentencesResponse (Result Task.Error (List PendingSentence))
    | SentenceChecked Int
    | ConfirmBatchClicked
    | UuidReceived String
    | GotConfirmBatchResponse (Result Task.Error ())
    | EditSentenceClicked Int
    | EditSentenceClose
    | EditSentenceConfirmClicked
    | GotEditSentenceResponse (Result Task.Error ())
    | UpdatedEditSentenceForm EditSentenceInput
    | DeleteSentenceClicked Int
    | DeleteSentenceClose


type EditSentenceInput
    = WordField String
    | SentenceField String
    | NewTagField String
    | TagAdded
    | TagRemoved String


update : Msg -> Model -> ( Model, Cmd Msg, Effect Msg )
update msg model =
    case msg of
        TabClicked MiningTab ->
            ( { model | tab = MiningTab }, Cmd.none, Effect.none )

        TabClicked PendingSentencesTab ->
            case model.getSentencesState of
                GetSentencesStale ->
                    ( { model
                        | tab = PendingSentencesTab
                        , getSentencesState = GetSentencesLoading
                      }
                    , Cmd.none
                    , getPendingSentencesRequest
                        |> Task.sheetsAttempt GotSentencesResponse
                        |> Effect.google
                    )

                _ ->
                    ( { model | tab = PendingSentencesTab }
                    , Cmd.none
                    , Effect.none
                    )

        ClipboardUpdated (Ok str) ->
            let
                trimmed =
                    String.trim str
            in
            if trimmed == model.sentence then
                ( model, Cmd.none, Effect.none )

            else
                ( { model
                    | sentence = trimmed
                    , sentenceWords = RegexExtra.sentenceSplit str
                    , selectedWord = Nothing
                    , definitionState = DefinitionWordNotSelected
                  }
                , Cmd.none
                , Effect.none
                )

        ClipboardUpdated (Err _) ->
            ( model, Cmd.none, Effect.none )

        WordSelected str ->
            ( { model
                | selectedWord = Just str
                , definitionState =
                    DefinitionLoading
              }
            , Cmd.none
            , Effect.wiktionary DefinitionFetchedResponse str
            )

        DefinitionFetchedResponse result ->
            case ( result, model.selectedWord ) of
                ( Ok definition, Just _ ) ->
                    ( { model | definitionState = DefinitionFetched definition }
                    , Cmd.none
                    , Effect.none
                    )

                ( Err _, Just _ ) ->
                    ( { model | definitionState = DefinitionNotFound }
                    , Cmd.none
                    , Effect.none
                    )

                _ ->
                    ( model, Cmd.none, Effect.none )

        BodyClicked ->
            ( model
            , PlatformTask.attempt ClipboardUpdated Port.readClipboard
            , Effect.none
            )

        MineClicked ->
            ( { model
                | sentence = ""
                , sentenceWords = []
                , selectedWord = Nothing
                , definitionState = DefinitionWordNotSelected
                , addSentenceState = AddSentenceLoading
                , getSentencesState = GetSentencesStale
              }
            , Cmd.none
            , addPendingSentenceRequest
                { word = model.selectedWord |> Maybe.withDefault ""
                , sentence = model.sentence
                , tags = []
                }
                |> Task.sheetsAttempt GotAddPendingSentenceResponse
                |> Effect.google
            )

        GotAddPendingSentenceResponse (Ok _) ->
            ( { model | addSentenceState = AddSentenceIdle }
            , Cmd.none
            , Effect.none
            )

        GotAddPendingSentenceResponse (Err err) ->
            ( { model | addSentenceState = AddSentenceIdle }
            , Cmd.none
            , Effect.toast
                { category = Toast.Error
                , title = "Failed to mine sentence"
                , description = Task.errorToMessage err
                }
            )

        GotSentencesResponse response ->
            case model.getSentencesState of
                GetSentencesLoading ->
                    ( { model
                        | getSentencesState = GetSentencesFetched response
                        , selectedSentences = Set.empty
                      }
                    , Cmd.none
                    , Effect.none
                    )

                -- if received when stale -> should ignore
                -- if received when fetched -> impossible state
                _ ->
                    ( model, Cmd.none, Effect.none )

        SentenceChecked index ->
            let
                setOperation =
                    if Set.member index model.selectedSentences then
                        Set.remove

                    else
                        Set.insert
            in
            ( { model
                | selectedSentences = setOperation index model.selectedSentences
              }
            , Cmd.none
            , Effect.none
            )

        ConfirmBatchClicked ->
            ( { model | confirmBatchState = ConfirmBatchLoading }
            , Cmd.none
            , Effect.uuid UuidReceived
            )

        UuidReceived uuid ->
            case model.getSentencesState of
                GetSentencesFetched (Ok sentences) ->
                    ( model
                    , Cmd.none
                    , confirmBatchRequest
                        { pendingSentences = sentences
                        , selectedSentences = model.selectedSentences
                        , batchId = uuid
                        }
                        |> Task.sheetsAttempt GotConfirmBatchResponse
                        |> Effect.google
                    )

                -- non-ok status should be impossible at this point
                _ ->
                    ( { model | confirmBatchState = ConfirmBatchIdle }
                    , Cmd.none
                    , Effect.none
                    )

        GotConfirmBatchResponse (Ok ()) ->
            ( { model
                | confirmBatchState = ConfirmBatchIdle
                , getSentencesState = GetSentencesLoading
                , selectedSentences = Set.empty
              }
            , Cmd.none
            , getPendingSentencesRequest
                |> Task.sheetsAttempt GotSentencesResponse
                |> Effect.google
            )

        GotConfirmBatchResponse (Err err) ->
            ( { model | confirmBatchState = ConfirmBatchIdle }
            , Cmd.none
            , Effect.toast
                { category = Toast.Error
                , title = "Failed to confirm batch"
                , description = Task.errorToMessage err
                }
            )

        EditSentenceClicked index ->
            ( { model
                | editSentenceForm =
                    case model.getSentencesState of
                        GetSentencesFetched (Ok sentences) ->
                            sentences
                                |> Array.fromList
                                |> Array.get index
                                |> Maybe.map
                                    (\sentence ->
                                        { isOpen = True
                                        , index = index
                                        , sentence = sentence
                                        , newTag = ""
                                        }
                                    )
                                |> Maybe.withDefault model.editSentenceForm

                        _ ->
                            model.editSentenceForm
              }
            , Cmd.none
            , Effect.none
            )

        UpdatedEditSentenceForm formInput ->
            ( { model
                | editSentenceForm =
                    let
                        form =
                            model.editSentenceForm

                        { sentence, newTag } =
                            form
                    in
                    case formInput of
                        WordField newWord ->
                            { form | sentence = { sentence | word = newWord } }

                        SentenceField newSentence ->
                            { form
                                | sentence =
                                    { sentence | sentence = newSentence }
                            }

                        NewTagField newNewTag ->
                            { form | newTag = newNewTag }

                        TagAdded ->
                            { form
                                | sentence =
                                    { sentence
                                        | tags =
                                            sentence.tags
                                                ++ [ String.trim newTag ]
                                    }
                                , newTag = ""
                            }

                        TagRemoved tag ->
                            { form
                                | sentence =
                                    { sentence
                                        | tags =
                                            List.filter
                                                ((/=) tag)
                                                sentence.tags
                                    }
                            }
              }
            , Cmd.none
            , Effect.none
            )

        EditSentenceConfirmClicked ->
            let
                maybeFrom =
                    case model.getSentencesState of
                        GetSentencesFetched (Ok sentences) ->
                            sentences
                                |> Array.fromList
                                |> Array.get model.editSentenceForm.index

                        _ ->
                            Nothing
            in
            case maybeFrom of
                Just from ->
                    ( { model | editSentenceState = EditSentenceLoading }
                    , Cmd.none
                    , editPendingSentenceRequest
                        from
                        model.editSentenceForm.sentence
                        |> Task.sheetsAttempt GotEditSentenceResponse
                        |> Effect.google
                    )

                Nothing ->
                    ( model, Cmd.none, Effect.none )

        GotEditSentenceResponse (Ok ()) ->
            let
                form =
                    model.editSentenceForm
            in
            ( { model
                | editSentenceState = EditSentenceIdle
                , editSentenceForm = { form | isOpen = False }
                , getSentencesState =
                    case model.getSentencesState of
                        GetSentencesFetched (Ok sentences) ->
                            (GetSentencesFetched << Ok) <|
                                List.indexedMap
                                    (\index sentence ->
                                        if index == model.editSentenceForm.index then
                                            model.editSentenceForm.sentence

                                        else
                                            sentence
                                    )
                                    sentences

                        _ ->
                            model.getSentencesState
              }
            , Cmd.none
            , Effect.none
            )

        GotEditSentenceResponse (Err err) ->
            ( { model | editSentenceState = EditSentenceIdle }
            , Cmd.none
            , Effect.toast
                { category = Toast.Error
                , title = "Failed to edit sentence"
                , description = Task.errorToMessage err
                }
            )

        EditSentenceClose ->
            let
                form =
                    model.editSentenceForm
            in
            ( { model
                | editSentenceForm = { form | isOpen = False }
              }
            , Cmd.none
            , Effect.none
            )

        DeleteSentenceClicked index ->
            ( { model
                | deleteSentenceForm =
                    case model.getSentencesState of
                        GetSentencesFetched (Ok sentences) ->
                            sentences
                                |> Array.fromList
                                |> Array.get index
                                |> Maybe.map
                                    (\sentence ->
                                        { isOpen = True
                                        , index = index
                                        , sentence = sentence
                                        }
                                    )
                                |> Maybe.withDefault model.deleteSentenceForm

                        _ ->
                            model.deleteSentenceForm
              }
            , Cmd.none
            , Effect.none
            )

        DeleteSentenceClose ->
            let
                form =
                    model.deleteSentenceForm
            in
            ( { model | deleteSentenceForm = { form | isOpen = False } }
            , Cmd.none
            , Effect.none
            )


type alias AddPendingSentence =
    { word : String
    , sentence : String
    , tags : List String
    }


addPendingSentenceRequest :
    AddPendingSentence
    -> Task.SheetsTask ()
addPendingSentenceRequest { word, sentence, tags } =
    Task.platform Time.now
        |> Task.andThen
            (\time ->
                Sheets.batchUpdateRequest
                    [ RequestAppendCells
                        { sheetId = Constants.subSheetId PendingSentences
                        , rows =
                            Sheets.requestRow
                                [ RequestString word
                                , RequestString sentence
                                , Sheets.tagsExtendedValue tags
                                , Sheets.timestampExtendedValue time
                                ]
                        , fields = "userEnteredValue"
                        }
                    , RequestInsertDimension
                        { range =
                            { sheetId = Constants.subSheetId MinedWords
                            , dimension = RequestRows
                            , startIndex = 1
                            , endIndex = 2
                            }
                        , inheritFromBefore = True
                        }
                    , RequestUpdateCells
                        { rows = Sheets.requestRow [ RequestString word ]
                        , fields = "userEnteredValue"
                        , range =
                            { sheetId = Constants.subSheetId MinedWords
                            , startRowIndex = Just 1
                            , endRowIndex = Just 2
                            , startColumnIndex = Just 0
                            , endColumnIndex = Just 1
                            }
                        }
                    , RequestDeleteDuplicates
                        { range =
                            { sheetId = Constants.subSheetId MinedWords
                            , startRowIndex = Just 1
                            , endRowIndex = Nothing
                            , startColumnIndex = Just 0
                            , endColumnIndex = Just 2
                            }
                        , comparisonColumns =
                            [ { sheetId = Constants.subSheetId MinedWords
                              , dimension = RequestColumns
                              , startIndex = 0
                              , endIndex = 1
                              }
                            ]
                        }
                    , RequestDeleteRange
                        { range =
                            { sheetId = Constants.subSheetId MinedWords
                            , startRowIndex = Just 1
                            , endRowIndex = Just 2
                            , startColumnIndex = Just 0
                            , endColumnIndex = Just 2
                            }
                        , dimension = RequestRows
                        }
                    ]
            )


getPendingSentencesRequest : Task.SheetsTask (List PendingSentence)
getPendingSentencesRequest =
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
                            ++ Constants.subSheetName PendingSentences
                            ++ "!A1:D,"
                            ++ "\"SELECT * WHERE A <> '' ORDER BY D DESC\")"
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
        [ Constants.subSheetName Query ++ "!A2:D" ]
        |> Task.map
            (.updatedSpreadsheet
                >> Model.fromGridData maybeConstructPendingSentence
            )


maybeConstructPendingSentence :
    List Sheets.ResponseCellData
    -> Maybe PendingSentence
maybeConstructPendingSentence =
    constructFromList PendingSentence
        >> field cellStringValue
        >> field cellStringValue
        >> field
            (cellStringValue
                >> Maybe.map (Decode.decodeString (Decode.list Decode.string))
                >> Maybe.andThen Result.toMaybe
            )
        >> field
            (cellNumberValue
                >> Maybe.map round
                >> Maybe.map Time.millisToPosix
            )
        >> extract


confirmBatchRequest :
    { pendingSentences : List PendingSentence
    , selectedSentences : Set Int
    , batchId : String
    }
    -> Task.SheetsTask ()
confirmBatchRequest { pendingSentences, selectedSentences, batchId } =
    let
        tagged =
            pendingSentences
                |> List.indexedMap (\index sentence -> ( index, sentence ))

        selectedPendingSentences =
            tagged
                |> List.filter
                    (\( index, _ ) -> Set.member index selectedSentences)

        deselectedPendingSentences =
            tagged
                |> List.filter
                    (\( index, _ ) -> not <| Set.member index selectedSentences)
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
                                        , Sheets.timestampExtendedValue time
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
                                        , Sheets.timestampExtendedValue time
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
                                        , Sheets.timestampExtendedValue time
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


editPendingSentenceRequest :
    PendingSentence
    -> PendingSentence
    -> Task.SheetsTask ()
editPendingSentenceRequest from to =
    Sheets.batchUpdateRequest
        [ RequestInsertDimension
            { range =
                { sheetId = Constants.subSheetId PendingSentences
                , dimension = RequestRows
                , startIndex = 1
                , endIndex = 2
                }
            , inheritFromBefore = True
            }
        , RequestUpdateCells
            { rows =
                Sheets.requestRow
                    [ RequestString from.word
                    , RequestString from.sentence
                    , Sheets.tagsExtendedValue from.tags
                    , Sheets.timestampExtendedValue from.addedAt
                    ]
            , fields = "userEnteredValue"
            , range =
                { sheetId = Constants.subSheetId PendingSentences
                , startRowIndex = Just 1
                , endRowIndex = Just 2
                , startColumnIndex = Just 0
                , endColumnIndex = Just 4
                }
            }
        , RequestDeleteDuplicates
            { range =
                { sheetId = Constants.subSheetId PendingSentences
                , startRowIndex = Just 1
                , endRowIndex = Nothing
                , startColumnIndex = Just 0
                , endColumnIndex = Just 4
                }
            , comparisonColumns =
                [ { sheetId = Constants.subSheetId PendingSentences
                  , dimension = RequestColumns
                  , startIndex = 0
                  , endIndex = 4
                  }
                ]
            }
        , RequestAppendCells
            { sheetId = Constants.subSheetId PendingSentences
            , rows =
                Sheets.requestRow
                    [ RequestString to.word
                    , RequestString to.sentence
                    , Sheets.tagsExtendedValue to.tags
                    , Sheets.timestampExtendedValue to.addedAt
                    ]
            , fields = "userEnteredValue"
            }
        , RequestDeleteRange
            { range =
                { sheetId = Constants.subSheetId PendingSentences
                , startRowIndex = Just 1
                , endRowIndex = Just 2
                , startColumnIndex = Just 0
                , endColumnIndex = Just 4
                }
            , dimension = RequestRows
            }
        ]


isBatchSelectionReady : Model -> Bool
isBatchSelectionReady model =
    case model.getSentencesState of
        GetSentencesFetched (Ok sentences) ->
            let
                selectedSize =
                    Set.size model.selectedSentences
            in
            selectedSize
                /= 0
                && (selectedSize
                        == List.length sentences
                        || selectedSize
                        == numSentencesToConfirmBatch
                   )

        _ ->
            False



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
            (List.concat
                [ [ classes
                        [ "flex"
                        , "shrink-0"
                        , "grow-0"
                        , "justify-center"
                        , "h-full"
                        ]
                  ]
                , case model.tab of
                    MiningTab ->
                        [ onClick BodyClicked ]

                    _ ->
                        []
                ]
            )
            [ div [ classes [ "tabs", "basis-md" ] ]
                [ nav
                    [ ariaOrientation "horizontal"
                    , class "w-full"
                    , role "tablist"
                    ]
                    [ button
                        [ ariaControls "tabs-mining"
                        , ariaSelected (model.tab == MiningTab)
                        , id "tabs-mining"
                        , role "tab"
                        , tabIndex "0"
                        , type_ "button"
                        , onClick (TabClicked MiningTab)
                        ]
                        [ text "Mining" ]
                    , button
                        [ ariaControls "tabs-pending-sentences"
                        , ariaSelected (model.tab == PendingSentencesTab)
                        , id "demo-tabs-with-panels-tab-2"
                        , role "tab"
                        , tabIndex "0"
                        , type_ "button"
                        , onClick (TabClicked PendingSentencesTab)
                        ]
                        [ text "Pending Sentences" ]
                    ]
                , div
                    [ ariaLabelledBy "tabs-mining"
                    , ariaSelected (model.tab == MiningTab)
                    , hidden (model.tab /= MiningTab)
                    , id "tabs-mining"
                    , role "tabpanel"
                    , tabIndex "-1"
                    ]
                    [ miningTabView model ]
                , div
                    [ ariaLabelledBy "tabs-pending-sentences"
                    , ariaSelected (model.tab == PendingSentencesTab)
                    , hidden (model.tab /= PendingSentencesTab)
                    , id "tabs-pending-sentences"
                    , role "tabpanel"
                    , tabIndex "-1"
                    ]
                    [ pendingSentencesTabView model ]
                ]
            , editSentenceDialogView model
            , deleteSentenceDialogView model
            ]
    }


editSentenceDialogView : Model -> Html Msg
editSentenceDialogView model =
    modalDialog
        model.editSentenceForm.isOpen
        EditSentenceClose
        [ classes
            [ "dialog"
            , "w-full"
            , "sm:max-w-[425px]"
            , "max-h-[612px]"
            ]
        ]
        [ article []
            [ header []
                [ h2 [] [ text "Edit sentence" ]
                ]
            , section []
                [ form [ classes [ "form", "grid", "gap-4" ] ] <|
                    [ div [ classes [ "grid", "gap-3" ] ]
                        [ label [ for "edit-sentence-word" ]
                            [ text "Word" ]
                        , input
                            [ type_ "text"
                            , id "edit-sentence-word"
                            , value model.editSentenceForm.sentence.word
                            , onInput (UpdatedEditSentenceForm << WordField)
                            ]
                            []
                        ]
                    , div [ classes [ "grid", "gap-3" ] ]
                        [ label [ for "edit-sentence-sentence" ]
                            [ text "Sentence" ]
                        , textarea
                            [ id "edit-sentence-sentence"
                            , onInput
                                (UpdatedEditSentenceForm << SentenceField)
                            ]
                            [ text model.editSentenceForm.sentence.sentence
                            ]
                        ]
                    , div [ classes [ "grid", "gap-3" ] ] <|
                        List.concat
                            [ [ label [ for "edit-sentence-tags" ]
                                    [ text "Tags" ]
                              ]
                            , case model.editSentenceForm.sentence.tags of
                                [] ->
                                    []

                                rest ->
                                    [ div
                                        [ classes
                                            [ "flex"
                                            , "flex-wrap"
                                            , "gap-1"
                                            ]
                                        ]
                                      <|
                                        List.map
                                            (\tag ->
                                                button
                                                    [ class "badge-outline"
                                                    , type_ "button"
                                                    , onClick
                                                        (UpdatedEditSentenceForm
                                                            (TagRemoved tag)
                                                        )
                                                    ]
                                                    [ text tag
                                                    , crossIcon []
                                                    ]
                                            )
                                            rest
                                    ]
                            , [ div [ class "flex gap-2" ]
                                    [ input
                                        [ type_ "text"
                                        , id "edit-sentence-tags"
                                        , placeholder "Add a tag..."
                                        , value model.editSentenceForm.newTag
                                        , onInput
                                            (UpdatedEditSentenceForm
                                                << NewTagField
                                            )
                                        ]
                                        []
                                    , button
                                        [ class "btn-icon-outline"
                                        , type_ "button"
                                        , disabled <|
                                            let
                                                tags =
                                                    model.editSentenceForm.sentence.tags

                                                newTag =
                                                    model.editSentenceForm.newTag
                                            in
                                            String.isEmpty newTag
                                                || List.any
                                                    ((==) (String.trim newTag))
                                                    tags
                                        , onClick
                                            (UpdatedEditSentenceForm TagAdded)
                                        ]
                                        [ plusIcon []
                                        ]
                                    ]
                              ]
                            ]
                    ]
                ]
            , footer []
                [ button
                    [ class "btn-outline"
                    , onClick EditSentenceClose
                    , disabled (model.editSentenceState == EditSentenceLoading)
                    ]
                    [ text "Cancel" ]
                , button
                    [ class "btn"
                    , onClick EditSentenceConfirmClicked
                    , disabled (model.editSentenceState == EditSentenceLoading)
                    ]
                  <|
                    List.concat
                        [ case model.editSentenceState of
                            EditSentenceIdle ->
                                []

                            EditSentenceLoading ->
                                [ loadingIcon [] ]
                        , [ text "Save changes" ]
                        ]
                ]
            , button
                [ type_ "button"
                , ariaLabel "Close dialog"
                , onClick EditSentenceClose
                ]
                [ crossIcon [] ]
            ]
        ]


deleteSentenceDialogView : Model -> Html Msg
deleteSentenceDialogView model =
    modalDialog
        model.deleteSentenceForm.isOpen
        DeleteSentenceClose
        [ classes
            [ "dialog"
            , "w-full"
            , "sm:max-w-[425px]"
            , "max-h-[612px]"
            ]
        ]
        [ article []
            [ header [] <|
                [ h2 [] [ text "Delete sentence" ]
                , p [] [ text "Are you sure you want to delete this sentence?" ]
                ]
            , section []
                [ sentenceView [] model.deleteSentenceForm.sentence ]
            , footer []
                [ button
                    [ class "btn-outline"
                    , onClick DeleteSentenceClose
                    ]
                    [ text "Cancel" ]
                , button [ class "btn-destructive" ]
                    [ text "Delete permanently" ]
                ]
            , button
                [ type_ "button"
                , ariaLabel "Close dialog"
                , onClick DeleteSentenceClose
                ]
                [ crossIcon [] ]
            ]
        ]


sentenceView : List (Attribute msg) -> PendingSentence -> Html msg
sentenceView attributes sentence =
    div (classes [ "grid", "gap-2" ] :: attributes) <|
        List.concat
            [ [ h2
                    [ classes
                        [ "text-sm"
                        , "leading-none"
                        , "font-medium"
                        , "[&:first-letter]:capitalize"
                        ]
                    ]
                    [ text sentence.word ]
              , p
                    [ classes
                        [ "text-sm"
                        , "[&:first-letter]:capitalize"
                        ]
                    ]
                    [ text sentence.sentence ]
              ]
            , case sentence.tags of
                [] ->
                    []

                rest ->
                    [ p
                        [ classes
                            [ "text-sm"
                            , "text-muted-foreground"
                            ]
                        ]
                        [ text <| String.join ", " rest ]
                    ]
            ]


onClick : msg -> Attribute msg
onClick msg =
    stopPropagationOn "click" <| Decode.succeed ( msg, True )



-- MINING TAB VIEW


miningTabView : Model -> Html Msg
miningTabView model =
    div [ classes [ "flex", "flex-col", "h-full", "mt-2" ] ]
        [ if List.length model.sentenceWords > 0 then
            div
                [ classes
                    [ "grow-1"
                    , "shrink-0"
                    , "overflow-auto"
                    , "basis-0"
                    ]
                , style "min-height" "0"
                ]
            <|
                List.concat
                    [ [ div
                            [ classes
                                [ "flex"
                                , "justify-center"
                                , "flex-wrap"
                                ]
                            ]
                        <|
                            List.map
                                (\word ->
                                    button
                                        [ classes
                                            [ "btn-outline"
                                            , "m-1"
                                            , "pt-1"
                                            , "pb-1"
                                            , "pr-2"
                                            , "pl-2"
                                            ]
                                        , onClick
                                            (WordSelected
                                                (String.toLower word)
                                            )
                                        ]
                                        [ text word ]
                                )
                                model.sentenceWords
                      ]
                    , dictionaryView model.selectedWord model.definitionState
                    ]

          else
            div
                [ classes
                    [ "grow-1"
                    , "shrink-1"
                    , "basis-auto"
                    , "flex"
                    , "justify-center"
                    , "items-center"
                    ]
                ]
                [ text "Click to paste and analyze a sentence!" ]
        , div [ classes [ "grow-0", "shrink-1", "mt-4" ] ]
            [ button
                [ classes [ "btn-primary", "w-full", "mb-4" ]
                , disabled
                    (model.selectedWord
                        == Nothing
                        || model.addSentenceState
                        == AddSentenceLoading
                    )
                , onClick MineClicked
                ]
                (List.concat
                    [ case model.addSentenceState of
                        AddSentenceIdle ->
                            []

                        AddSentenceLoading ->
                            [ loadingIcon [] ]
                    , [ text "Mine Sentence" ]
                    ]
                )
            ]
        ]


dictionaryView : Maybe String -> DefinitionState -> List (Html Msg)
dictionaryView maybeWord definitionState =
    case maybeWord of
        Just word ->
            List.concat
                [ [ div [ classes [ "card", "p-1", "mt-4" ] ]
                        [ header [ classes [ "p-0", "flex", "justify-center" ] ]
                            [ p [ class "[&:first-letter]:capitalize" ]
                                [ text word ]
                            ]
                        ]
                  ]
                , case definitionState of
                    DefinitionWordNotSelected ->
                        []

                    DefinitionLoading ->
                        [ div [ classes [ "card", "p-3", "mt-4", "gap-2" ] ]
                            [ header [ class "p-0" ]
                                [ div
                                    [ classes
                                        [ "bg-accent"
                                        , "animate-pulse"
                                        , "rounded-md"
                                        , "h-4"
                                        , "w-2/3"
                                        ]
                                    ]
                                    []
                                ]
                            , section [ class "p-0" ]
                                [ div
                                    [ classes
                                        [ "bg-accent"
                                        , "animate-pulse"
                                        , "rounded-md"
                                        , "h-16"
                                        , "w-full"
                                        ]
                                    ]
                                    []
                                ]
                            ]
                        ]

                    DefinitionFetched (Usages []) ->
                        [ notFoundView ]

                    DefinitionFetched usages ->
                        usagesView usages

                    DefinitionNotFound ->
                        [ notFoundView ]
                ]

        Nothing ->
            []


notFoundView : Html Msg
notFoundView =
    div [ classes [ "alert-destructive", "mt-4" ] ]
        [ warningCircleIcon []
        , h2 [] [ text "Unable to find word definition." ]
        ]


usagesView : Wiktionary.Usages -> List (Html Msg)
usagesView (Usages usages) =
    List.indexedMap definitionsView usages


definitionsView : Int -> Wiktionary.Definitions -> Html Msg
definitionsView index (Definitions definitions) =
    div [ classes [ "card", "p-3", "mt-4", "gap-0" ] ]
        [ header [ classes [ "p-0", "flex", "justify-center" ] ]
            [ text ("Etimology " ++ String.fromInt (index + 1)) ]
        , section [ class "pl-0 pr-0" ]
            [ ul [ classes [ "list-decimal", "list-inside" ] ] <|
                List.map
                    (\definition ->
                        definitionView definition
                    )
                    definitions
            ]
        ]


definitionView : Wiktionary.Definition -> Html Msg
definitionView definition =
    li [ class "[&:not(:last-child):has(.form-usages)]:mb-3" ] <|
        List.concat
            [ Regex.split RegexExtra.newLines definition.text
                |> List.map (text << String.trim)
            , List.map formUsagesView definition.formUsages
            ]


formUsagesView : Wiktionary.FormUsages -> Html Msg
formUsagesView { word, usages } =
    let
        (Usages formUsages) =
            usages
    in
    div [] <|
        List.indexedMap
            (\definitionIndex (Definitions definitions) ->
                div
                    [ classes <|
                        [ "gap-2"
                        , "flex"
                        , "flex-row"
                        , "items-start"
                        , "justify-between"
                        , "rounded-lg"
                        , "border"
                        , "p-3"
                        , "shadow-xs"
                        , "mt-3"
                        , "form-usages"
                        ]
                    ]
                    [ div [ classes [ "flex", "flex-col", "gap-0.5" ] ]
                        [ p [ class "leading-normal" ]
                            [ text <|
                                "Etimology "
                                    ++ String.fromInt (definitionIndex + 1)
                                    ++ " for "
                                    ++ word
                            ]
                        , p
                            [ classes
                                [ "text-muted-foreground"
                                , "text-sm"
                                , "pl-4"
                                ]
                            ]
                            [ ul [ class "list-disc" ] <|
                                List.map
                                    (\definition ->
                                        li [ class "list-disc" ]
                                            (Regex.split
                                                RegexExtra.newLines
                                                definition.text
                                                |> List.map
                                                    (text << String.trim)
                                            )
                                    )
                                    definitions
                            ]
                        ]
                    ]
            )
            formUsages



-- PENDING SENTENCES TAB VIEW


pendingSentencesTabView : Model -> Html Msg
pendingSentencesTabView model =
    div [ classes [ "flex flex-col h-full mt-2" ] ]
        [ div [ classes [ "grow-1", "shrink-0", "overflow-auto", "basis-0" ] ]
            [ case model.getSentencesState of
                GetSentencesStale ->
                    div [] []

                GetSentencesLoading ->
                    div []
                        [ pendingSentenceSkeletonView "w-[7%]" "w-[85%]"
                        , pendingSentenceSkeletonView "w-[10%]" "w-[70%]"
                        , pendingSentenceSkeletonView "w-[5%]" "w-[95%]"
                        ]

                GetSentencesFetched (Ok []) ->
                    div [ classes [ "alert" ] ]
                        [ infoIcon []
                        , h2 [] [ text "No sentences have yet been mined." ]
                        ]

                GetSentencesFetched (Ok sentences) ->
                    div []
                        (List.indexedMap (pendingSentenceView model) sentences)

                GetSentencesFetched (Err err) ->
                    div [ classes [ "alert-destructive" ] ]
                        [ warningCircleIcon []
                        , h2 [] [ text "Unable to fetch pending sentences." ]
                        , section [] [ text <| Task.errorToMessage err ]
                        ]
            ]
        , div [ classes [ "grow-0", "shrink-1", "mt-4" ] ]
            [ button
                [ classes [ "btn-primary", "w-full", "mb-4" ]
                , disabled
                    (model.getSentencesState
                        == GetSentencesLoading
                        || not (isBatchSelectionReady model)
                        || model.confirmBatchState
                        == ConfirmBatchLoading
                    )
                , onClick ConfirmBatchClicked
                ]
                (List.concat
                    [ if model.confirmBatchState == ConfirmBatchLoading then
                        [ loadingIcon [] ]

                      else
                        []
                    , [ text "Confirm Batch" ]
                    ]
                )
            ]
        ]


pendingSentenceSkeletonView : String -> String -> Html Msg
pendingSentenceSkeletonView wordSize sentenceSize =
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
        [ div
            [ classes
                [ "bg-accent"
                , "animate-pulse"
                , "rounded-md"
                , "h-4"
                , wordSize
                ]
            ]
            []
        , div
            [ classes
                [ "bg-accent"
                , "animate-pulse"
                , "rounded-md"
                , "h-4"
                , sentenceSize
                ]
            ]
            []
        ]


pendingSentenceView : Model -> Int -> PendingSentence -> Html Msg
pendingSentenceView model index sentence =
    label
        [ classes
            [ "flex"
            , "items-start"
            , "gap-3"
            , "border"
            , "p-3"
            , "hover:bg-accent/50"
            , "rounded-lg"
            , "has-[input[type='checkbox']:checked]:border-blue-600"
            , "has-[input[type='checkbox']:checked]:bg-blue-50"
            , "dark:has-[input[type='checkbox']:checked]:border-blue-900"
            , "dark:has-[input[type='checkbox']:checked]:bg-blue-950"
            , "has-[input[type='checkbox']:disabled]:opacity-50"
            , "[&:not(:first-child)]:mt-4"
            ]
        ]
        [ input
            (let
                isChecked =
                    Set.member index model.selectedSentences
             in
             [ classes
                [ "input checked:bg-blue-600"
                , "checked:border-blue-600"
                , "dark:checked:bg-blue-700"
                , "dark:checked:border-blue-700"
                , "checked:after:bg-white"
                ]
             , type_ "checkbox"
             , checked isChecked
             , onCheck (\_ -> SentenceChecked index)
             , disabled (not isChecked && isBatchSelectionReady model)
             ]
            )
            []
        , div [ classes [ "grow-1", "grid", "gap-2" ] ] <|
            [ div [ classes [ "flex", "items-center" ] ]
                [ sentenceView [ class "grow-1" ] sentence
                , button
                    [ class "btn-sm-icon-ghost"
                    , onClick (EditSentenceClicked index)
                    ]
                    [ squarePenIcon [] ]
                , button
                    [ class "btn-sm-icon-ghost text-(--destructive)"
                    , onClick (DeleteSentenceClicked index)
                    ]
                    [ trashIcon [] ]
                ]
            ]
        ]



-- CONSTANTS


numSentencesToConfirmBatch : Int
numSentencesToConfirmBatch =
    10
