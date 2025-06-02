module Page.Auth exposing (..)

import Api.Action as Action exposing (Action)
import Html exposing (Html, div, text)
import Route
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session }


init : Session -> ( Model, Cmd Msg, Action Msg )
init session =
    ( { session = session }
    , Cmd.none
    , Action.googleInitialize AuthenticateCompleted
    )



-- UPDATE


type Msg
    = AuthenticateCompleted


update : Msg -> Model -> ( Model, Cmd Msg, Action Msg )
update msg ({ session } as model) =
    case msg of
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
            [ text "Authorizing with Google..."
            ]
    }
