module Page.Mining exposing (..)

import Api exposing (Update(..))
import Html exposing (Html, button, div, li, text, ul)
import Html.Events exposing (onClick)



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { ids = [] }
    , Cmd.none
    )



-- MODEL


type alias Model =
    { ids : List String }



-- UPDATE


type Msg
    = SendRequest
    | GotRequest (Result Api.Error (Maybe String))


update : Msg -> Model -> ( Model, Api.Update Msg )
update msg model =
    case msg of
        SendRequest ->
            ( model
            , Api.gapiGetAppFolderId GotRequest
            )

        GotRequest result ->
            case result of
                Ok id ->
                    ( { ids = model.ids ++ [ Maybe.withDefault "?" id ] }, None )

                Err _ ->
                    ( model, None )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button
            [ onClick SendRequest ]
            [ text "Initialize" ]
        , ul
            []
            (List.map
                (\id -> li [] [ text id ])
                model.ids
            )
        ]
