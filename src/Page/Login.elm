module Page.Login exposing (..)

import Api.Action as Action exposing (Action(..))
import Api.Google.ParamTask as ParamTask
import Api.Google.Requests as GoogleRequests
import Html exposing (Html, button, div, span, text)
import Html.Events exposing (onClick)
import Http



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
    | TestRequestPressed
    | TestRequestReceived (Result Http.Error GoogleRequests.DriveResponseFileList)


update : Msg -> Model -> ( Model, Cmd Msg, Action Msg )
update msg model =
    case msg of
        AuthenticatePressed ->
            ( model
            , Cmd.none
            , Action.googleInitialize AuthenticateCompleted
            )

        AuthenticateCompleted ->
            ( Authenticated, Cmd.none, Action.none )

        TestRequestPressed ->
            ( model
            , Cmd.none
            , Action.google <|
                ParamTask.attempt
                    TestRequestReceived
                    GoogleRequests.getAppFolderId
            )

        TestRequestReceived result ->
            let
                _ =
                    Debug.log "result:" result
            in
            ( model, Cmd.none, Action.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        (if model /= Authenticated then
            [ button [ onClick AuthenticatePressed ] [ text "Authorize with Google" ]
            ]

         else
            [ button [ onClick TestRequestPressed ] [ text "Send test request" ] ]
        )
