module Page.PendingSentences exposing
    ( Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

import Api.Action as Action exposing (Action)
import Html exposing (Html, div, text)
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session }


init : Session -> ( Model, Cmd Msg, Action Msg )
init session =
    ( { session = session }
    , Cmd.none
    , Action.none
    )



-- UPDATE


type Msg
    = Foo


update : Msg -> Model -> ( Model, Cmd Msg, Action Msg )
update msg model =
    ( model, Cmd.none, Action.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Pending Sentences"
    , content =
        div [] [ text "sentences page" ]
    }
