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
        , ariaInvalid
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
import Icon.Tag exposing (tagIcon)
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
    , mining : MiningState
    , overview : OverviewState
    }


type Tab
    = TabMining
    | TabOverview


type alias MiningState =
    { sentence : String
    , words : List String
    , selected : Maybe String
    , definitionState : DefinitionState
    , addState : AddState
    }


type DefinitionState
    = DefinitionWordNotSelected
    | DefinitionLoading
    | DefinitionFinished Usages
    | DefinitionNotFound


type AddState
    = AddIdle
    | AddLoading


type alias OverviewState =
    { selected : Set Int
    , editForm : EditForm
    , deleteForm : DeleteForm
    , getState : GetState
    , editState : EditState
    , deleteState : DeleteState
    , confirmState : ConfirmState
    }


type alias EditForm =
    { isOpen : Bool
    , index : Int
    , sentence : PendingSentence
    , newTag : String
    , validation : EditFormValidation
    }


type alias EditFormValidation =
    { word : Maybe String
    , sentence : Maybe String
    }


type alias DeleteForm =
    { isOpen : Bool
    , index : Int
    , sentence : PendingSentence
    }


type GetState
    = GetStale
    | GetLoading
    | GetFinished (Result Task.Error (List PendingSentence))


type EditState
    = EditIdle
    | EditLoading


type DeleteState
    = DeleteIdle
    | DeleteLoading


type ConfirmState
    = ConfirmIdle
    | ConfirmLoading


init : Session -> ( Model, Cmd Msg, Effect Msg )
init session =
    ( { session = session
      , tab = TabMining
      , mining =
            { sentence = ""
            , words = []
            , selected = Nothing
            , definitionState = DefinitionWordNotSelected
            , addState = AddIdle
            }
      , overview =
            { selected = Set.empty
            , editForm =
                { isOpen = False
                , index = 0
                , sentence =
                    { word = ""
                    , sentence = ""
                    , tags = []
                    , addedAt = Time.millisToPosix 0
                    }
                , newTag = ""
                , validation =
                    { word = Nothing
                    , sentence = Nothing
                    }
                }
            , deleteForm =
                { isOpen = False
                , index = 0
                , sentence =
                    { word = ""
                    , sentence = ""
                    , tags = []
                    , addedAt = Time.millisToPosix 0
                    }
                }
            , getState = GetStale
            , editState = EditIdle
            , deleteState = DeleteIdle
            , confirmState = ConfirmIdle
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
    | DefinitionFetched (Result Http.Error Wiktionary.Usages)
    | MineClicked
    | AddFetched (Result Task.Error ())
    | GetFetched (Result Task.Error (List PendingSentence))
    | SentenceChecked Int
    | BatchConfirmClicked
    | UuidReceived String
    | BatchFetched (Result Task.Error ())
    | EditClicked Int
    | EditFormUpdated EditFormInput
    | EditDialogClosed
    | EditConfirmClicked
    | EditFetched (Result Task.Error ())
    | DeleteClicked Int
    | DeleteDialogClosed
    | DeleteConfirmClicked
    | DeleteFetched (Result Task.Error ())


type EditFormInput
    = WordField String
    | SentenceField String
    | NewTagField String
    | TagAdded
    | TagRemoved String


update : Msg -> Model -> ( Model, Cmd Msg, Effect Msg )
update msg ({ mining, overview } as model) =
    case msg of
        TabClicked TabMining ->
            ( { model | tab = TabMining }, Cmd.none, Effect.none )

        TabClicked TabOverview ->
            case overview.getState of
                GetStale ->
                    ( { model
                        | tab = TabOverview
                        , overview =
                            { overview | getState = GetLoading }
                      }
                    , Cmd.none
                    , getRequest
                        |> Task.sheetsAttempt GetFetched
                        |> Effect.google
                    )

                _ ->
                    ( { model | tab = TabOverview }
                    , Cmd.none
                    , Effect.none
                    )

        ClipboardUpdated (Ok str) ->
            let
                trimmed =
                    String.trim str
            in
            if trimmed == mining.sentence then
                ( model, Cmd.none, Effect.none )

            else
                ( { model
                    | mining =
                        { mining
                            | sentence = trimmed
                            , words = RegexExtra.sentenceSplit str
                            , selected = Nothing
                            , definitionState = DefinitionWordNotSelected
                        }
                  }
                , Cmd.none
                , Effect.none
                )

        ClipboardUpdated (Err _) ->
            ( model, Cmd.none, Effect.none )

        WordSelected str ->
            ( { model
                | mining =
                    { mining
                        | selected = Just str
                        , definitionState = DefinitionLoading
                    }
              }
            , Cmd.none
            , Effect.wiktionary DefinitionFetched str
            )

        DefinitionFetched result ->
            case ( result, mining.selected ) of
                ( Ok definition, Just _ ) ->
                    ( { model
                        | mining =
                            { mining
                                | definitionState =
                                    DefinitionFinished definition
                            }
                      }
                    , Cmd.none
                    , Effect.none
                    )

                ( Err _, Just _ ) ->
                    ( { model
                        | mining =
                            { mining | definitionState = DefinitionNotFound }
                      }
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
                | mining =
                    { mining
                        | sentence = ""
                        , words = []
                        , selected = Nothing
                        , definitionState = DefinitionWordNotSelected
                        , addState = AddLoading
                    }
                , overview = { overview | getState = GetStale }
              }
            , Cmd.none
            , addRequest
                { word = mining.selected |> Maybe.withDefault ""
                , sentence = mining.sentence
                , tags = []
                }
                |> Task.sheetsAttempt AddFetched
                |> Effect.google
            )

        AddFetched (Ok _) ->
            ( { model | mining = { mining | addState = AddIdle } }
            , Cmd.none
            , Effect.none
            )

        AddFetched (Err err) ->
            ( { model | mining = { mining | addState = AddIdle } }
            , Cmd.none
            , Effect.toast
                { category = Toast.Error
                , title = "Failed to mine sentence"
                , description = Task.errorToMessage err
                }
            )

        GetFetched response ->
            case overview.getState of
                GetLoading ->
                    ( { model
                        | overview =
                            { overview
                                | getState = GetFinished response
                                , selected = Set.empty
                            }
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
                    if Set.member index overview.selected then
                        Set.remove

                    else
                        Set.insert
            in
            ( { model
                | overview =
                    { overview
                        | selected = setOperation index overview.selected
                    }
              }
            , Cmd.none
            , Effect.none
            )

        BatchConfirmClicked ->
            ( { model
                | overview = { overview | confirmState = ConfirmLoading }
              }
            , Cmd.none
            , Effect.uuid UuidReceived
            )

        UuidReceived uuid ->
            case overview.getState of
                GetFinished (Ok sentences) ->
                    ( model
                    , Cmd.none
                    , confirmBatchRequest
                        { sentences = sentences
                        , selectedSentences = overview.selected
                        , batchId = uuid
                        }
                        |> Task.sheetsAttempt BatchFetched
                        |> Effect.google
                    )

                -- non-ok status should be impossible at this point
                _ ->
                    ( { model
                        | overview = { overview | confirmState = ConfirmIdle }
                      }
                    , Cmd.none
                    , Effect.none
                    )

        BatchFetched (Ok ()) ->
            ( { model
                | overview =
                    { overview
                        | confirmState = ConfirmIdle
                        , getState = GetLoading
                        , selected = Set.empty
                    }
              }
            , Cmd.none
            , getRequest
                |> Task.sheetsAttempt GetFetched
                |> Effect.google
            )

        BatchFetched (Err err) ->
            ( { model
                | overview = { overview | confirmState = ConfirmIdle }
              }
            , Cmd.none
            , Effect.toast
                { category = Toast.Error
                , title = "Failed to confirm batch"
                , description = Task.errorToMessage err
                }
            )

        EditClicked index ->
            ( { model
                | overview =
                    { overview
                        | editForm =
                            case overview.getState of
                                GetFinished (Ok sentences) ->
                                    sentences
                                        |> Array.fromList
                                        |> Array.get index
                                        |> Maybe.map
                                            (\sentence ->
                                                { isOpen = True
                                                , index = index
                                                , sentence = sentence
                                                , newTag = ""
                                                , validation =
                                                    { word = Nothing
                                                    , sentence = Nothing
                                                    }
                                                }
                                            )
                                        |> Maybe.withDefault overview.editForm

                                _ ->
                                    overview.editForm
                    }
              }
            , Cmd.none
            , Effect.none
            )

        EditFormUpdated formInput ->
            ( { model
                | overview =
                    { overview
                        | editForm =
                            let
                                form =
                                    overview.editForm

                                { sentence, newTag } =
                                    form

                                { validation } =
                                    form
                            in
                            case formInput of
                                WordField newWord ->
                                    { form
                                        | sentence =
                                            { sentence
                                                | word = newWord
                                            }
                                        , validation =
                                            { validation
                                                | word =
                                                    case newWord of
                                                        "" ->
                                                            Just "Word field cannot be empty"

                                                        _ ->
                                                            Nothing
                                            }
                                    }

                                SentenceField newSentence ->
                                    { form
                                        | sentence =
                                            { sentence
                                                | sentence = newSentence
                                            }
                                        , validation =
                                            { validation
                                                | sentence =
                                                    case newSentence of
                                                        "" ->
                                                            Just "Sentence field cannot be empty"

                                                        _ ->
                                                            Nothing
                                            }
                                    }

                                NewTagField newNewTag ->
                                    { form | newTag = newNewTag }

                                TagAdded ->
                                    { form
                                        | sentence =
                                            { sentence
                                                | tags =
                                                    sentence.tags
                                                        ++ [ String.trim newTag
                                                           ]
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
              }
            , Cmd.none
            , Effect.none
            )

        EditConfirmClicked ->
            let
                maybeFrom =
                    case overview.getState of
                        GetFinished (Ok sentences) ->
                            sentences
                                |> Array.fromList
                                |> Array.get overview.editForm.index

                        _ ->
                            Nothing
            in
            case maybeFrom of
                Just from ->
                    ( { model
                        | overview = { overview | editState = EditLoading }
                      }
                    , Cmd.none
                    , editRequest
                        from
                        overview.editForm.sentence
                        |> Task.sheetsAttempt EditFetched
                        |> Effect.google
                    )

                Nothing ->
                    ( model, Cmd.none, Effect.none )

        EditFetched (Ok ()) ->
            let
                form =
                    overview.editForm

                { sentence } =
                    form
            in
            ( { model
                | overview =
                    { overview
                        | editState = EditIdle
                        , editForm = { form | isOpen = False }
                        , getState =
                            case overview.getState of
                                GetFinished (Ok sentences) ->
                                    (GetFinished << Ok) <|
                                        listTransformAt
                                            form.index
                                            { sentence
                                                | word =
                                                    String.toLower sentence.word
                                            }
                                            sentences

                                _ ->
                                    overview.getState
                    }
              }
            , Cmd.none
            , Effect.none
            )

        EditFetched (Err err) ->
            ( { model | overview = { overview | editState = EditIdle } }
            , Cmd.none
            , Effect.toast
                { category = Toast.Error
                , title = "Failed to edit sentence"
                , description = Task.errorToMessage err
                }
            )

        EditDialogClosed ->
            let
                form =
                    overview.editForm
            in
            ( { model
                | overview =
                    { overview | editForm = { form | isOpen = False } }
              }
            , Cmd.none
            , Effect.none
            )

        DeleteClicked index ->
            ( { model
                | overview =
                    { overview
                        | deleteForm =
                            case overview.getState of
                                GetFinished (Ok sentences) ->
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
                                        |> Maybe.withDefault overview.deleteForm

                                _ ->
                                    overview.deleteForm
                    }
              }
            , Cmd.none
            , Effect.none
            )

        DeleteDialogClosed ->
            let
                form =
                    overview.deleteForm
            in
            ( { model
                | overview =
                    { overview | deleteForm = { form | isOpen = False } }
              }
            , Cmd.none
            , Effect.none
            )

        DeleteConfirmClicked ->
            ( { model | overview = { overview | deleteState = DeleteLoading } }
            , Cmd.none
            , removeRequest
                overview.deleteForm.sentence
                |> Task.sheetsAttempt DeleteFetched
                |> Effect.google
            )

        DeleteFetched (Ok ()) ->
            let
                form =
                    overview.deleteForm
            in
            ( { model
                | overview =
                    { overview
                        | deleteState = DeleteIdle
                        , deleteForm = { form | isOpen = False }
                        , getState =
                            case overview.getState of
                                GetFinished (Ok sentences) ->
                                    (GetFinished << Ok)
                                        (sentences
                                            |> List.indexedMap
                                                (\index sentence ->
                                                    if index == form.index then
                                                        Nothing

                                                    else
                                                        Just sentence
                                                )
                                            |> List.filterMap identity
                                        )

                                _ ->
                                    overview.getState
                    }
              }
            , Cmd.none
            , Effect.none
            )

        DeleteFetched (Err err) ->
            ( { model | overview = { overview | deleteState = DeleteIdle } }
            , Cmd.none
            , Effect.toast
                { category = Toast.Error
                , title = "Failed to delete sentence"
                , description = Task.errorToMessage err
                }
            )


type alias AddPendingSentence =
    { word : String
    , sentence : String
    , tags : List String
    }


addRequest :
    AddPendingSentence
    -> Task.SheetsTask ()
addRequest { word, sentence, tags } =
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


getRequest : Task.SheetsTask (List PendingSentence)
getRequest =
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
    { sentences : List PendingSentence
    , selectedSentences : Set Int
    , batchId : String
    }
    -> Task.SheetsTask ()
confirmBatchRequest { sentences, selectedSentences, batchId } =
    let
        tagged =
            sentences
                |> List.indexedMap (\index sentence -> ( index, sentence ))

        sentencesToAdd =
            tagged
                |> List.filter
                    (\( index, _ ) -> Set.member index selectedSentences)

        sentencesToIgnore =
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
                            sentencesToAdd
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
                            sentencesToAdd
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
                            sentencesToIgnore
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


editRequest :
    PendingSentence
    -> PendingSentence
    -> Task.SheetsTask ()
editRequest from to =
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
                    [ RequestString <| String.toLower to.word
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


removeRequest :
    PendingSentence
    -> Task.SheetsTask ()
removeRequest sentence =
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
                    [ RequestString sentence.word
                    , RequestString sentence.sentence
                    , Sheets.tagsExtendedValue sentence.tags
                    , Sheets.timestampExtendedValue sentence.addedAt
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


listTransformAt : Int -> a -> List a -> List a
listTransformAt index newValue =
    List.indexedMap
        (\itemIndex value ->
            if itemIndex == index then
                newValue

            else
                value
        )


isSelectionReady : Model -> Bool
isSelectionReady { overview } =
    case overview.getState of
        GetFinished (Ok sentences) ->
            let
                selectedSize =
                    Set.size overview.selected
            in
            selectedSize
                /= 0
                && (selectedSize
                        == List.length sentences
                        || selectedSize
                        == numSentencesPerBatch
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
                    TabMining ->
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
                        , ariaSelected (model.tab == TabMining)
                        , id "tabs-mining"
                        , role "tab"
                        , tabIndex "0"
                        , type_ "button"
                        , onClick (TabClicked TabMining)
                        ]
                        [ text "Mining" ]
                    , button
                        [ ariaControls "tabs-overview"
                        , ariaSelected (model.tab == TabOverview)
                        , id "tabs-overview"
                        , role "tab"
                        , tabIndex "0"
                        , type_ "button"
                        , onClick (TabClicked TabOverview)
                        ]
                        [ text "Overview" ]
                    ]
                , div
                    [ ariaLabelledBy "tabs-mining"
                    , ariaSelected (model.tab == TabMining)
                    , hidden (model.tab /= TabMining)
                    , id "tabs-mining"
                    , role "tabpanel"
                    , tabIndex "-1"
                    ]
                    [ miningTabView model ]
                , div
                    [ ariaLabelledBy "tabs-overview"
                    , ariaSelected (model.tab == TabOverview)
                    , hidden (model.tab /= TabOverview)
                    , id "tabs-overview"
                    , role "tabpanel"
                    , tabIndex "-1"
                    ]
                    [ overviewTabView model ]
                ]
            , editDialogView model
            , deleteDialogView model
            ]
    }


editDialogView : Model -> Html Msg
editDialogView { overview } =
    modalDialog
        overview.editForm.isOpen
        EditDialogClosed
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
                    [ div [ classes [ "grid", "gap-3" ] ] <|
                        List.concat
                            [ [ label [ for "edit-sentence-word" ]
                                    [ text "Word" ]
                              , input
                                    [ type_ "text"
                                    , id "edit-sentence-word"
                                    , value overview.editForm.sentence.word
                                    , onInput
                                        (EditFormUpdated << WordField)
                                    , ariaInvalid
                                        (overview.editForm.validation.word
                                            /= Nothing
                                        )
                                    ]
                                    []
                              ]
                            , case overview.editForm.validation.word of
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
                            [ [ label [ for "edit-sentence-sentence" ]
                                    [ text "Sentence" ]
                              , textarea
                                    [ id "edit-sentence-sentence"
                                    , onInput
                                        (EditFormUpdated
                                            << SentenceField
                                        )
                                    , ariaInvalid
                                        (overview.editForm.validation.sentence
                                            /= Nothing
                                        )
                                    ]
                                    [ text overview.editForm.sentence.sentence ]
                              ]
                            , case overview.editForm.validation.sentence of
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
                            [ [ label [ for "edit-sentence-tags" ]
                                    [ text "Tags" ]
                              ]
                            , case overview.editForm.sentence.tags of
                                [] ->
                                    []

                                rest ->
                                    [ div
                                        [ classes
                                            [ "flex"
                                            , "flex-wrap"
                                            , "gap-2"
                                            ]
                                        ]
                                      <|
                                        List.map
                                            (\tag ->
                                                button
                                                    [ classes
                                                        [ "badge"
                                                        , "py-0"
                                                        , "px-1"
                                                        ]
                                                    , type_ "button"
                                                    , onClick
                                                        (EditFormUpdated
                                                            (TagRemoved tag)
                                                        )
                                                    ]
                                                    [ tagIcon []
                                                    , text tag
                                                    , crossIcon []
                                                    ]
                                            )
                                            rest
                                    ]
                            , [ div [ classes [ "flex", "gap-2" ] ]
                                    [ input
                                        [ type_ "text"
                                        , id "edit-sentence-tags"
                                        , placeholder "Add a tag..."
                                        , value overview.editForm.newTag
                                        , onInput
                                            (EditFormUpdated
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
                                                    overview.editForm.sentence.tags

                                                newTag =
                                                    overview.editForm.newTag
                                            in
                                            String.isEmpty newTag
                                                || List.any
                                                    ((==) (String.trim newTag))
                                                    tags
                                        , onClick
                                            (EditFormUpdated TagAdded)
                                        ]
                                        [ plusIcon [] ]
                                    ]
                              ]
                            ]
                    ]
                ]
            , footer []
                [ button
                    [ class "btn-outline"
                    , onClick EditDialogClosed
                    , disabled (overview.editState == EditLoading)
                    ]
                    [ text "Cancel" ]
                , button
                    [ class "btn"
                    , onClick EditConfirmClicked
                    , disabled
                        (overview.editState
                            == EditLoading
                            || overview.editForm.validation.word
                            /= Nothing
                            || overview.editForm.validation.sentence
                            /= Nothing
                        )
                    ]
                  <|
                    List.concat
                        [ case overview.editState of
                            EditIdle ->
                                []

                            EditLoading ->
                                [ loadingIcon [] ]
                        , [ text "Save changes" ]
                        ]
                ]
            , button
                [ type_ "button"
                , ariaLabel "Close dialog"
                , onClick EditDialogClosed
                ]
                [ crossIcon [] ]
            ]
        ]


deleteDialogView : Model -> Html Msg
deleteDialogView { overview } =
    modalDialog
        overview.deleteForm.isOpen
        DeleteDialogClosed
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
                [ sentenceView [] overview.deleteForm.sentence ]
            , footer []
                [ button
                    [ class "btn-outline"
                    , onClick DeleteDialogClosed
                    , disabled (overview.deleteState == DeleteLoading)
                    ]
                    [ text "Cancel" ]
                , button
                    [ class "btn-destructive"
                    , onClick DeleteConfirmClicked
                    , disabled (overview.deleteState == DeleteLoading)
                    ]
                  <|
                    List.concat
                        [ case overview.deleteState of
                            DeleteIdle ->
                                []

                            DeleteLoading ->
                                [ loadingIcon [] ]
                        , [ text "Delete permanently" ]
                        ]
                ]
            , button
                [ type_ "button"
                , ariaLabel "Close dialog"
                , onClick DeleteDialogClosed
                ]
                [ crossIcon [] ]
            ]
        ]


sentenceView : List (Attribute Msg) -> PendingSentence -> Html Msg
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
                    [ div [ classes [ "flex", "flex-wrap", "gap-2" ] ] <|
                        List.map
                            (\tag ->
                                div [ classes [ "badge", "py-0", "px-1" ] ]
                                    [ tagIcon []
                                    , text tag
                                    ]
                            )
                            rest
                    ]
            ]


onClick : msg -> Attribute msg
onClick msg =
    stopPropagationOn "click" <| Decode.succeed ( msg, True )



-- MINING TAB VIEW


miningTabView : Model -> Html Msg
miningTabView { mining } =
    div [ classes [ "flex", "flex-col", "h-full", "mt-2" ] ]
        [ if List.length mining.words > 0 then
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
                                            (WordSelected (String.toLower word))
                                        ]
                                        [ text word ]
                                )
                                mining.words
                      ]
                    , dictionaryView mining.selected mining.definitionState
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
            [ div [ classes [ "flex", "flex-wrap", "gap-2", "mb-4" ] ]
                [ button
                    [ classes [ "badge" ]
                    , type_ "button"
                    ]
                    [ text "Edit tags..." ]
                ]
            , button
                [ classes [ "btn-primary", "w-full", "mb-4" ]
                , disabled
                    (mining.selected
                        == Nothing
                        || mining.addState
                        == AddLoading
                    )
                , onClick MineClicked
                ]
                (List.concat
                    [ case mining.addState of
                        AddIdle ->
                            []

                        AddLoading ->
                            [ loadingIcon [] ]
                    , [ text "Mine sentence" ]
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

                    DefinitionFinished (Usages []) ->
                        [ notFoundView ]

                    DefinitionFinished usages ->
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
            [ text ("Etymology " ++ String.fromInt (index + 1)) ]
        , section [ classes [ "pl-0", "pr-0" ] ]
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
                                "Etymology "
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



-- OVERVIEW TAB VIEW


overviewTabView : Model -> Html Msg
overviewTabView ({ overview } as model) =
    div [ classes [ "flex", "flex-col", "h-full", "mt-2" ] ]
        [ div [ classes [ "grow-1", "shrink-0", "overflow-auto", "basis-0" ] ]
            [ case overview.getState of
                GetStale ->
                    div [] []

                GetLoading ->
                    div []
                        [ sentenceSkeletonView "w-[7%]" "w-[85%]"
                        , sentenceSkeletonView "w-[10%]" "w-[70%]"
                        , sentenceSkeletonView "w-[5%]" "w-[95%]"
                        ]

                GetFinished (Ok []) ->
                    div [ class "alert" ]
                        [ infoIcon []
                        , h2 [] [ text "No sentences have yet been mined." ]
                        ]

                GetFinished (Ok sentences) ->
                    div []
                        (List.indexedMap (overviewSentenceView model) sentences)

                GetFinished (Err err) ->
                    div [ class "alert-destructive" ]
                        [ warningCircleIcon []
                        , h2 [] [ text "Unable to fetch pending sentences." ]
                        , section [] [ text <| Task.errorToMessage err ]
                        ]
            ]
        , div [ classes [ "grow-0", "shrink-1", "mt-4" ] ]
            [ button
                [ classes [ "btn-primary", "w-full", "mb-4" ]
                , disabled
                    (overview.getState
                        == GetLoading
                        || not (isSelectionReady model)
                        || overview.confirmState
                        == ConfirmLoading
                    )
                , onClick BatchConfirmClicked
                ]
                (List.concat
                    [ if overview.confirmState == ConfirmLoading then
                        [ loadingIcon [] ]

                      else
                        []
                    , [ text "Confirm Batch" ]
                    ]
                )
            ]
        ]


sentenceSkeletonView : String -> String -> Html Msg
sentenceSkeletonView wordSize sentenceSize =
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


overviewSentenceView : Model -> Int -> PendingSentence -> Html Msg
overviewSentenceView ({ overview } as model) index sentence =
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
                    Set.member index overview.selected
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
             , disabled (not isChecked && isSelectionReady model)
             ]
            )
            []
        , div [ classes [ "grow-1", "grid", "gap-2" ] ] <|
            [ div [ classes [ "flex", "items-center" ] ]
                [ sentenceView [ class "grow-1" ] sentence
                , button
                    [ class "btn-sm-icon-ghost"
                    , onClick (EditClicked index)
                    ]
                    [ squarePenIcon [] ]
                , button
                    [ class "btn-sm-icon-ghost text-(--destructive)"
                    , onClick (DeleteClicked index)
                    ]
                    [ trashIcon [] ]
                ]
            ]
        ]



-- CONSTANTS


numSentencesPerBatch : Int
numSentencesPerBatch =
    10
