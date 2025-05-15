module Main exposing (..)

import Api.Action as Action exposing (Action)
import Api.Google as Google exposing (Msg(..))
import Api.Wiktionary as Wiktionary exposing (Msg(..))
import Browser
import Html exposing (Html)
import OutMsg exposing (OutMsg)
import Page.Login as Login exposing (Msg(..))
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
    , wiktionaryModel : Wiktionary.Model Msg
    , page : Page
    }


type Page
    = Login Login.Model
    | Mining Mining.Model


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( googleModel, googleCmd ) =
            Google.init ()

        ( wiktionaryModel, wiktionaryCmd ) =
            Wiktionary.init ()

        ( loginModel, loginCmd ) =
            Login.init ()
    in
    ( { googleModel = googleModel
      , wiktionaryModel = wiktionaryModel
      , page = Login loginModel
      }
    , Cmd.batch
        [ Cmd.map GotGoogleMsg googleCmd
        , Cmd.map GotWiktionaryMsg wiktionaryCmd
        , Cmd.map GotLoginMsg loginCmd
        ]
    )



-- UPDATE


type Msg
    = GotGoogleMsg (Google.Msg Msg)
    | GotWiktionaryMsg (Wiktionary.Msg Msg)
    | GotLoginMsg Login.Msg
    | GotMiningMsg Mining.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( GotGoogleMsg subMsg, _ ) ->
            Google.update subMsg model.googleModel
                |> updateWithApi
                    (\googleModel -> { model | googleModel = googleModel })
                    GotGoogleMsg

        ( GotWiktionaryMsg subMsg, _ ) ->
            Wiktionary.update subMsg model.wiktionaryModel
                |> updateWithApi
                    (\wiktionaryModel ->
                        { model
                            | wiktionaryModel = wiktionaryModel
                        }
                    )
                    GotWiktionaryMsg

        ( GotLoginMsg subMsg, Login subModel ) ->
            Login.update subMsg subModel
                |> updateWithPage Login GotLoginMsg model

        ( GotMiningMsg subMsg, Mining subModel ) ->
            Mining.update subMsg subModel
                |> updateWithPage Mining GotMiningMsg model

        ( _, _ ) ->
            ( model, Cmd.none )


updateWithApi :
    (subModel -> Model)
    -> (subMsg -> Msg)
    -> ( subModel, Cmd subMsg, OutMsg Msg )
    -> ( Model, Cmd Msg )
updateWithApi toModel toMsg result =
    result
        |> Triple.mapFirst toModel
        |> (\( updatedModel, subMsg, outMsg ) ->
                outMsg
                    |> OutMsg.toList
                    |> resolveOutMsgUpdates updatedModel Cmd.none
                    |> Tuple.mapSecond
                        (\updateCmd ->
                            Cmd.batch
                                [ updateCmd
                                , Cmd.map toMsg subMsg
                                ]
                        )
           )


resolveOutMsgUpdates : Model -> Cmd Msg -> List Msg -> ( Model, Cmd Msg )
resolveOutMsgUpdates model cmd msgs =
    case msgs of
        msg :: rest ->
            let
                ( newModel, newCmd ) =
                    update msg model
            in
            resolveOutMsgUpdates newModel (Cmd.batch [ cmd, newCmd ]) rest

        [] ->
            ( model, cmd )


updateWithPage :
    (subModel -> Page)
    -> (subMsg -> Msg)
    -> Model
    -> ( subModel, Cmd subMsg, Action subMsg )
    -> ( Model, Cmd Msg )
updateWithPage toPage toMsg model result =
    result
        |> Triple.mapFirst
            (\updatedSubModel -> { model | page = toPage updatedSubModel })
        |> (\( updatedSubModel, cmd, action ) ->
                Action.map toMsg action
                    |> performApiAction updatedSubModel
                    |> Tuple.mapSecond
                        (\googleActionCmd ->
                            Cmd.batch
                                [ googleActionCmd
                                , Cmd.map toMsg cmd
                                ]
                        )
           )


performApiAction : Model -> Action Msg -> ( Model, Cmd Msg )
performApiAction model action =
    Action.match action
        { onNone = ( model, Cmd.none )
        , onGoogle =
            \googleAction ->
                update (GotGoogleMsg <| Google.SentAction googleAction) model
        , onWiktionary =
            \request ->
                update
                    (GotWiktionaryMsg <| Wiktionary.SentRequest request)
                    model
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map GotGoogleMsg (Google.subscriptions model.googleModel)
        , Sub.map
            GotWiktionaryMsg
            (Wiktionary.subscriptions model.wiktionaryModel)
        , case model.page of
            Login subModel ->
                Sub.none

            Mining subModel ->
                Sub.map GotMiningMsg (Mining.subscriptions subModel)
        ]



-- VIEW


view : Model -> Html Msg
view model =
    case model.page of
        Login subModel ->
            Html.map GotLoginMsg (Login.view subModel)

        Mining subModel ->
            Html.map GotMiningMsg (Mining.view subModel)
