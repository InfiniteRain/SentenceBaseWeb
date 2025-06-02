module Page.Mining exposing (..)

import Api.Action as Action exposing (Action(..))
import Api.Google.Constants as Constants exposing (SubSheet(..))
import Api.Google.ParamTask as ParamTask exposing (ParamTask)
import Api.Google.Requests as Requests
    exposing
        ( SheetRequestBatchUpdateKind(..)
        , SheetRequestExtendedValue(..)
        , sheetRequestRow
        )
import Api.Wiktionary as Wiktionary exposing (Definitions(..), Usages(..))
import Html exposing (Attribute, Html, br, button, div, li, span, text, ul)
import Html.Attributes exposing (class, disabled, style)
import Html.Events exposing (onClick, stopPropagationOn)
import Http
import Iso8601
import Json.Decode as Decode
import Port
import Regex
import RegexExtra
import Session exposing (Session)
import Task
import TaskPort
import Time



-- MODEL


type alias Model =
    { session : Session
    , sentence : String
    , sentenceWords : List String
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
    | GotAddPendingSentenceResponse (Result Requests.Error ())


update : Msg -> Model -> ( Model, Cmd Msg, Action Msg )
update msg model =
    case msg of
        ClipboardUpdated (Ok str) ->
            if str == model.sentence then
                ( model, Cmd.none, Action.none )

            else
                ( { model
                    | sentence =
                        str
                            |> String.trim
                    , sentenceWords =
                        str
                            |> String.trim
                            |> Regex.split RegexExtra.space
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
            case result of
                Ok definition ->
                    ( { model | definitionState = Fetched definition }
                    , Cmd.none
                    , Action.none
                    )

                Err _ ->
                    ( { model | definitionState = NotFound }
                    , Cmd.none
                    , Action.none
                    )

        BodyClicked ->
            ( model
            , Task.attempt ClipboardUpdated Port.readClipboard
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
                }
                |> ParamTask.attempt GotAddPendingSentenceResponse
                |> Action.google
            )

        GotAddPendingSentenceResponse _ ->
            ( model, Cmd.none, Action.none )


type alias PendingSentence =
    { word : String
    , sentence : String
    }


addPendingSentenceRequest : PendingSentence -> ParamTask Requests.Error ()
addPendingSentenceRequest { word, sentence } sheetId =
    Time.now
        |> Task.andThen
            (\time ->
                Requests.sheetBatchUpdateRequest
                    [ AppendCells
                        { sheetId = Constants.subSheetId PendingSentences
                        , rows =
                            sheetRequestRow
                                [ StringValue word
                                , StringValue sentence
                                , StringValue <| Iso8601.fromTime time
                                ]
                        , fields = "userEnteredValue"
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
                , [ button
                        [ disabled (model.selectedWord == Nothing)
                        , onClick MineClicked
                        ]
                        [ text "Mine" ]
                  ]
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
