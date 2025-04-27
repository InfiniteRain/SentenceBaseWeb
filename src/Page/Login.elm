module Page.Login exposing (..)

import Api exposing (Action(..))
import Api.Google.Requests as GoogleRequests
import Html exposing (Html, button, div, span, text)
import Html.Events exposing (onClick)



-- MODEL


type Model
    = Unauthenticated
    | Authenticated


init : () -> ( Model, Cmd Msg )
init _ =
    ( Unauthenticated, Cmd.none )



-- UPDATE


type Msg
    = AuthenticatePressed
    | AuthenticateCompleted


update : Msg -> Model -> ( Model, Cmd Msg, Api.Action Msg )
update msg model =
    case msg of
        AuthenticatePressed ->
            ( model
            , Cmd.none
            , Google (GoogleRequests.Initialize AuthenticateCompleted)
            )

        AuthenticateCompleted ->
            ( Authenticated, Cmd.none, None )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        (if model /= Authenticated then
            [ button [ onClick AuthenticatePressed ] [ text "Authorize with Google" ]
            ]

         else
            [ span [] [ text "Authenticated" ] ]
        )
