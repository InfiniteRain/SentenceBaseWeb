port module Page.Mining exposing (..)

import Api.Google as Google exposing (Action(..))
import Html exposing (Html, br, button, div, li, span, text, ul)
import Html.Events exposing (onClick)
import Regex



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { sentenceWords = []
      , selectedWord = Nothing
      }
    , Cmd.none
    )



-- PORTS


port clipboardPort : (String -> msg) -> Sub msg



-- MODEL


type alias Model =
    { sentenceWords : List String
    , selectedWord : Maybe String
    }



-- UPDATE


type Msg
    = ClipboardUpdated String
    | WordSelected String


update : Msg -> Model -> ( Model, Cmd Msg, Google.Action Msg )
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
            , Cmd.none
            , None
            )


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
            ++ [ span [] [ text "Wij hebben een serieus probleem" ] ]
        )
