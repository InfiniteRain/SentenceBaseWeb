port module Page.Mining exposing (..)

import Api.Action as Action exposing (Action(..))
import Api.Wiktionary as Wiktionary exposing (Definitions(..), Usages(..))
import Html exposing (Html, br, button, div, li, span, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Regex
import RegexExtra
import Session exposing (Session)



-- PORTS


port clipboardPort : (String -> msg) -> Sub msg



-- MODEL


type alias Model =
    { session : Session
    , sentenceWords : List String
    , selectedWord : Maybe String
    , definition : Maybe Usages
    , pressed : Int
    , received : Int
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , sentenceWords = []
      , selectedWord = Nothing
      , definition = Nothing
      , pressed = 0
      , received = 0
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ClipboardUpdated String
    | WordSelected String
    | DefinitionFetched (Result Http.Error Wiktionary.Usages)


update : Msg -> Model -> ( Model, Cmd Msg, Action Msg )
update msg model =
    case msg of
        ClipboardUpdated str ->
            ( { model
                | sentenceWords =
                    str
                        |> String.trim
                        |> Regex.split RegexExtra.space
                , selectedWord = Nothing
              }
            , Cmd.none
            , Action.none
            )

        WordSelected str ->
            ( { model
                | selectedWord = Just str
                , pressed = model.pressed + 1
              }
            , Cmd.none
            , Action.wiktionary DefinitionFetched str
            )

        DefinitionFetched result ->
            case result of
                Ok definition ->
                    ( { model
                        | definition = Just definition
                        , received = model.received + 1
                      }
                    , Cmd.none
                    , Action.none
                    )

                Err _ ->
                    ( { model
                        | definition = Nothing
                        , received = model.received + 1
                      }
                    , Cmd.none
                    , Action.none
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    clipboardPort ClipboardUpdated



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Mining"
    , content =
        div
            []
            (List.concat
                [ List.map (\word -> button [ onClick (WordSelected (String.toLower word)) ] [ text word ]) model.sentenceWords
                , model.selectedWord
                    |> Maybe.map
                        (\word ->
                            [ br [] []
                            , span [] [ text word ]
                            ]
                        )
                    |> Maybe.withDefault []
                , [ br [] [] ]
                , model.definition
                    |> Maybe.map (\definition -> [ usagesView definition ])
                    |> Maybe.withDefault []
                , [ span [] [ text <| "pressed: " ++ String.fromInt model.pressed ++ ", received " ++ String.fromInt model.received ]
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
    case usages of
        Usages formUsages ->
            List.indexedMap
                (\index (Definitions definitions) ->
                    div []
                        [ div [ class "etimology" ]
                            [ text <| "Etimology " ++ String.fromInt (index + 1) ++ " for " ++ word
                            , ul [] <|
                                List.map
                                    (\definition ->
                                        li []
                                            (Regex.split RegexExtra.newLines definition.text
                                                |> List.map (\line -> div [] [ text (String.trim line) ])
                                            )
                                    )
                                    definitions
                            ]
                        ]
                )
                formUsages
