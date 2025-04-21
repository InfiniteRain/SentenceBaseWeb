module Page.Mining exposing (..)

import Gapi exposing (Action(..))
import Html exposing (Html, br, button, div, li, span, text, ul)
import Html.Events exposing (onClick)



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { ids = [], clicks = 0 }
    , Cmd.none
    )



-- MODEL


type alias Model =
    { ids : List String
    , clicks : Int
    }



-- UPDATE


type Msg
    = SendRequest
    | GotRequest (Result Gapi.Error (Maybe String))


update : Msg -> Model -> ( Model, Cmd Msg, Gapi.Action Msg )
update msg model =
    case msg of
        SendRequest ->
            ( { model | clicks = model.clicks + 1 }
            , Cmd.none
            , Gapi (Gapi.gapiGetAppFolderId GotRequest)
            )

        GotRequest result ->
            case result of
                Ok id ->
                    ( { model | ids = model.ids ++ [ Maybe.withDefault "?" id ] }
                    , Cmd.none
                    , None
                    )

                Err _ ->
                    ( model
                    , Cmd.none
                    , None
                    )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ span [] [ text (String.fromInt model.clicks) ]
        , br [] []
        , button
            [ onClick SendRequest ]
            [ text "Initialize" ]
        , ul
            []
            (List.map
                (\id -> li [] [ text id ])
                model.ids
            )
        ]
