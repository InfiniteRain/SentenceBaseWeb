module Main exposing (..)

import Api exposing (Action(..))
import Api.Google as Google exposing (Msg(..))
import Browser
import Html exposing (Html)
import Page.Mining as Mining exposing (Msg(..))
import Triple



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { googleModel : Google.Model Msg
    , page : Page
    }


type Page
    = Mining Mining.Model


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( googleModel, googleCmd ) =
            Google.init ()

        ( miningModel, miningCmd ) =
            Mining.init ()
    in
    ( { googleModel = googleModel
      , page = Mining miningModel
      }
    , Cmd.batch
        [ Cmd.map GotMiningMsg miningCmd
        , Cmd.map GotGoogleMsg googleCmd
        ]
    )



-- UPDATE


type Msg
    = GotGoogleMsg (Google.Msg Msg)
    | GotMiningMsg Mining.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( GotGoogleMsg subMsg, _ ) ->
            Google.update subMsg model.googleModel
                |> updateApiWith
                    (\updatedModel -> { model | googleModel = updatedModel })
                    GotGoogleMsg

        ( GotMiningMsg subMsg, Mining subModel ) ->
            Mining.update subMsg subModel
                |> updatePageWith Mining GotMiningMsg model


updateApiWith :
    (subModel -> Model)
    -> (subMsg -> Msg)
    -> ( subModel, Cmd subMsg, Maybe Msg )
    -> ( Model, Cmd Msg )
updateApiWith toModel toMsg result =
    result
        |> Triple.mapFirst toModel
        |> (\( updatedModel, googleCmd, maybeOutMsg ) ->
                maybeOutMsg
                    |> Maybe.map
                        (\outMsg ->
                            update outMsg updatedModel
                        )
                    |> Maybe.withDefault ( updatedModel, Cmd.none )
                    |> Tuple.mapSecond
                        (\updateCmd ->
                            Cmd.batch
                                [ updateCmd
                                , Cmd.map toMsg googleCmd
                                ]
                        )
           )


updatePageWith :
    (subModel -> Page)
    -> (subMsg -> Msg)
    -> Model
    -> ( subModel, Cmd subMsg, Api.Action subMsg )
    -> ( Model, Cmd Msg )
updatePageWith toPage toMsg model result =
    result
        |> Triple.mapFirst
            (\updatedSubModel -> { model | page = toPage updatedSubModel })
        |> (\( updatedSubModel, cmd, action ) ->
                Api.mapAction toMsg action
                    |> performApiAction updatedSubModel
                    |> Tuple.mapSecond
                        (\googleActionCmd ->
                            Cmd.batch
                                [ googleActionCmd
                                , Cmd.map toMsg cmd
                                ]
                        )
           )


performApiAction : Model -> Api.Action Msg -> ( Model, Cmd Msg )
performApiAction model action =
    case action of
        Google request ->
            update (GotGoogleMsg (SentRequest request)) model

        None ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map GotGoogleMsg (Google.subscriptions model.googleModel)
        , case model.page of
            Mining subModel ->
                Sub.map GotMiningMsg (Mining.subscriptions subModel)
        ]



-- VIEW


view : Model -> Html Msg
view model =
    case model.page of
        Mining subModel ->
            Html.map GotMiningMsg (Mining.view subModel)
