module Page.Auth exposing (..)

import Api.Action as Action exposing (Action)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Route
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session }
    , Cmd.none
    )



-- UPDATE


type Msg
    = AuthenticatePressed
    | AuthenticateCompleted


update : Msg -> Model -> ( Model, Cmd Msg, Action Msg )
update msg ({ session } as model) =
    case msg of
        AuthenticatePressed ->
            ( model
            , Cmd.none
            , Action.googleInitialize AuthenticateCompleted
            )

        AuthenticateCompleted ->
            ( { model | session = Session.authenticate session }
            , Route.navigate (Session.navKey session) Route.Mining
            , Action.none
            )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view _ =
    { title = "Authentication"
    , content =
        div []
            [ button [ onClick AuthenticatePressed ]
                [ text "Authorize with Google" ]
            ]
    }
