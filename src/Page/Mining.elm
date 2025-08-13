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
import Api.Wiktionary as Wiktionary exposing (Definitions(..), Usages(..))
import Basecoat
    exposing
        ( ariaControls
        , ariaLabelledBy
        , ariaOrientation
        , ariaSelected
        , classes
        , role
        , tabIndex
        )
import Html
    exposing
        ( Attribute
        , Html
        , button
        , div
        , h2
        , header
        , li
        , nav
        , p
        , section
        , text
        , ul
        )
import Html.Attributes
    exposing
        ( class
        , disabled
        , hidden
        , id
        , style
        , type_
        )
import Html.Events exposing (onClick, stopPropagationOn)
import Http
import Icon.Warn exposing (warnIcon)
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
    , tab : Tab
    , sentence : String
    , sentenceWords : List String
    , tagsInput : String
    , selectedWord : Maybe String
    , definitionState : DefinitionState
    , addRequestState : AddRequestState
    }


type Tab
    = MiningTab
    | PendingSentencesTab


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
      , tab = MiningTab
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
    = TabClicked Tab
    | BodyClicked
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
        TabClicked tab ->
            ( { model | tab = tab }
            , Cmd.none
            , Action.none
            )

        ClipboardUpdated (Ok str) ->
            let
                trimmed =
                    String.trim str
            in
            if trimmed == model.sentence then
                ( model, Cmd.none, Action.none )

            else
                ( { model
                    | sentence = trimmed
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

        GotAddPendingSentenceResponse _ ->
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


addPendingSentenceRequest :
    PendingSentence
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
                                , Sheets.iso8601ExtendedValue time
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
            [ classes
                [ "flex"
                , "shrink-0"
                , "grow-0"
                , "justify-center"
                , "h-full"
                ]
            , onClick BodyClicked
            ]
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
            ]
    }


miningTabView : Model -> Html Msg
miningTabView model =
    div [ classes [ "flex flex-col h-full mt-2" ] ]
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
                    [ [ div [ classes [ "flex", "justify-center", "flex-wrap" ] ] <|
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
                , disabled (model.selectedWord == Nothing)
                , onClick MineClicked
                ]
                [ text "Mine Sentence" ]
            ]
        ]


dictionaryView : Maybe String -> DefinitionState -> List (Html Msg)
dictionaryView maybeWord definitionState =
    case maybeWord of
        Just word ->
            List.concat
                [ [ div [ classes [ "card", "p-1", "mt-4" ] ]
                        [ header [ classes [ "p-0", "flex", "justify-center" ] ]
                            [ p [ class "uppercase" ] [ text word ] ]
                        ]
                  ]
                , case definitionState of
                    WordNotSelected ->
                        []

                    Loading ->
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

                    Fetched (Usages []) ->
                        [ notFoundView ]

                    Fetched usages ->
                        usagesView usages

                    NotFound ->
                        [ notFoundView ]
                ]

        Nothing ->
            []


notFoundView : Html Msg
notFoundView =
    div [ classes [ "alert-destructive", "mt-4" ] ]
        [ warnIcon []
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
            [ ul [ classes [ "list-decimal", "list-inside" ] ]
                (let
                    len =
                        List.length definitions
                 in
                 List.indexedMap
                    (\definitionIndex definition ->
                        definitionView (definitionIndex == len - 1) definition
                    )
                    definitions
                )
            ]
        ]


definitionView : Bool -> Wiktionary.Definition -> Html Msg
definitionView isLast definition =
    li [] <|
        List.concat
            [ Regex.split RegexExtra.newLines definition.text
                |> List.map (text << String.trim)
            , List.map (formUsagesView isLast) definition.formUsages
            ]


formUsagesView : Bool -> Wiktionary.FormUsages -> Html Msg
formUsagesView isLastDefinition { word, usages } =
    let
        (Usages formUsages) =
            usages

        containerClasses =
            if isLastDefinition then
                []

            else
                [ "mb-3" ]
    in
    div [ classes containerClasses ] <|
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


pendingSentencesTabView : Model -> Html Msg
pendingSentencesTabView model =
    div [] []


onClick : msg -> Attribute msg
onClick msg =
    stopPropagationOn "click" <| Decode.succeed ( msg, True )
