port module Page.Mining exposing (..)

import Api exposing (Action(..))
import Api.Google as Google exposing (getAppFolderId)
import Api.Wiktionary as Wiktionary
import Html exposing (Html, br, button, div, li, span, text, ul)
import Html.Events exposing (onClick)
import Html.Parser exposing (Node)
import Http
import Parser
import Regex



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { sentenceWords = []
      , selectedWord = Nothing
      , parsed = Nothing
      }
    , Cmd.none
    )



-- PORTS


port clipboardPort : (String -> msg) -> Sub msg



-- MODEL


type alias Model =
    { sentenceWords : List String
    , selectedWord : Maybe String
    , parsed : Maybe (Result (List Parser.DeadEnd) (List Html.Parser.Node))
    }



-- UPDATE


type Msg
    = ClipboardUpdated String
    | WordSelected String
    | DefinitionFetched (Result Http.Error Wiktionary.WiktionaryResponse)
    | Test (Result Google.Error (Maybe String))


update : Msg -> Model -> ( Model, Cmd Msg, Api.Action Msg )
update msg model =
    case msg of
        ClipboardUpdated str ->
            ( { model
                | sentenceWords =
                    str
                        |> String.trim
                        |> Regex.split space
                , selectedWord = Nothing
              }
            , Cmd.none
            , None
            )

        WordSelected str ->
            ( { model
                | selectedWord = Just str
              }
              -- , Wiktionary.wiktionaryRequest str DefinitionFetched
            , Cmd.none
            , Google (getAppFolderId Test)
            )

        DefinitionFetched result ->
            case result of
                Ok r ->
                    case r.nl of
                        Just nl ->
                            case List.head nl of
                                Just h ->
                                    case List.head h.definitions of
                                        Just d ->
                                            let
                                                a =
                                                    Html.Parser.run d.definition

                                                _ =
                                                    Debug.log "html" (Html.Parser.run d.definition)
                                            in
                                            ( { model | parsed = Just a }, Cmd.none, None )

                                        Nothing ->
                                            ( model, Cmd.none, None )

                                Nothing ->
                                    ( model, Cmd.none, None )

                        Nothing ->
                            ( model, Cmd.none, None )

                Err _ ->
                    ( model, Cmd.none, None )

        Test a ->
            let
                _ =
                    Debug.log "here" a
            in
            ( model, Cmd.none, None )


space : Regex.Regex
space =
    Regex.fromString "\\s+"
        |> Maybe.withDefault Regex.never



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    clipboardPort ClipboardUpdated



-- VIEW


view : Model -> Html Msg
view model =
    div
        []
        (List.map (\word -> button [ onClick (WordSelected (String.toLower word)) ] [ text word ]) model.sentenceWords
            ++ [ br [] []
               , span [] [ text (Maybe.withDefault "" model.selectedWord) ]
               ]
            ++ List.repeat 10 (br [] [])
            ++ [ span [] [ text "Wij hebben een serieus probleem" ]
               , br [] []
               , span [] [ text "zag" ]
               ]
        )
