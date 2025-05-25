module Main exposing (..)

import Api.Action as Action exposing (Action)
import Api.Google as Google exposing (Msg(..))
import Api.Wiktionary as Wiktionary exposing (Msg(..))
import Browser
import Browser.Navigation as Nav
import Html exposing (Html)
import OutMsg exposing (OutMsg)
import Page.Auth as Auth exposing (Msg(..))
import Page.Mining as Mining exposing (Msg(..))
import Route exposing (Route(..))
import Session exposing (Session)
import Triple
import Url exposing (Url)



-- API SETUP


type ApiMsg
    = GotGoogleMsg (Google.Msg Msg)
    | GotWiktionaryMsg (Wiktionary.Msg Msg)


type alias ApiModel =
    { google : Google.Model Msg
    , wiktionary : Wiktionary.Model Msg
    }


updateApi : ApiMsg -> ApiModel -> ApiUpdate
updateApi apiMsg apiModel =
    case apiMsg of
        GotGoogleMsg subMsg ->
            updateWithApi
                subMsg
                apiModel.google
                Google.update
                GotGoogleMsg
                (\model -> { apiModel | google = model })

        GotWiktionaryMsg subMsg ->
            updateWithApi
                subMsg
                apiModel.wiktionary
                Wiktionary.update
                GotWiktionaryMsg
                (\model -> { apiModel | wiktionary = model })


initApi : ( ApiModel, Cmd Msg )
initApi =
    let
        ( googleModel, googleCmd ) =
            Google.init ()

        ( wiktionaryModel, wiktionaryCmd ) =
            Wiktionary.init ()
    in
    ( { google = googleModel
      , wiktionary = wiktionaryModel
      }
    , Cmd.batch
        [ initApiCmd GotGoogleMsg googleCmd
        , initApiCmd GotWiktionaryMsg wiktionaryCmd
        ]
    )



-- MODEL


type alias Model =
    { api : ApiModel
    , page : Page
    }


type Page
    = Auth Auth.Model
    | Mining Mining.Model


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ navKey =
    let
        ( apiModel, apiCmd ) =
            initApi

        session =
            Session.create navKey

        ( authModel, authCmd ) =
            Auth.init session
    in
    ( { api = apiModel
      , page = Auth authModel
      }
    , Cmd.batch
        [ apiCmd
        , Cmd.map GotAuthMsg authCmd
        , Route.navigate navKey Route.Auth
        ]
    )


initApiCmd : (msg -> ApiMsg) -> Cmd msg -> Cmd Msg
initApiCmd toApiMsg cmd =
    Cmd.map (GotApiMsg << toApiMsg) cmd


routeToPage : Maybe Route -> Session -> ( Page, Cmd Msg )
routeToPage maybeRoute session =
    case maybeRoute of
        Just Route.Auth ->
            Auth.init session
                |> Tuple.mapBoth Auth (Cmd.map GotAuthMsg)

        Just Route.Mining ->
            Mining.init session
                |> Tuple.mapBoth Mining (Cmd.map GotMiningMsg)

        Nothing ->
            Auth.init session
                |> Tuple.mapBoth Auth (Cmd.map GotAuthMsg)


pageToSession : Page -> Session
pageToSession page =
    case page of
        Auth model ->
            model.session

        Mining model ->
            model.session



-- UPDATE


type Msg
    = UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | GotApiMsg ApiMsg
    | GotAuthMsg Auth.Msg
    | GotMiningMsg Mining.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        session =
            pageToSession model.page
    in
    case ( msg, model.page ) of
        ( UrlChanged url, _ ) ->
            routeToPage (Route.fromUrl url) session
                |> Tuple.mapFirst (\page -> { model | page = page })

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl
                        (Session.navKey session)
                        (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( GotApiMsg apiMsg, _ ) ->
            updateApi apiMsg model.api model

        ( GotAuthMsg subMsg, Auth subModel ) ->
            Auth.update subMsg subModel
                |> updateWithPage Auth GotAuthMsg model

        ( GotMiningMsg subMsg, Mining subModel ) ->
            Mining.update subMsg subModel
                |> updateWithPage Mining GotMiningMsg model

        ( _, _ ) ->
            ( model, Cmd.none )


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


performApiAction : Model -> Action Msg -> ( Model, Cmd Msg )
performApiAction model action =
    Action.match action
        { onNone = ( model, Cmd.none )
        , onGoogle =
            \googleAction ->
                update
                    (GotApiMsg <|
                        GotGoogleMsg <|
                            Google.SentAction googleAction
                    )
                    model
        , onWiktionary =
            \request ->
                update
                    (GotApiMsg <|
                        GotWiktionaryMsg <|
                            Wiktionary.SentRequest request
                    )
                    model
        }


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


type alias ApiUpdate =
    Model -> ( Model, Cmd Msg )


updateWithApi :
    msg
    -> model
    -> (msg -> model -> ( model, Cmd msg, OutMsg Msg ))
    -> (msg -> ApiMsg)
    -> (model -> ApiModel)
    -> ApiUpdate
updateWithApi subMsg subModel updateFn toMsg toModel model =
    updateFn subMsg subModel
        |> Triple.mapFirst (\m -> { model | api = toModel m })
        |> (\( updatedModel, msg, outMsg ) ->
                outMsg
                    |> OutMsg.toList
                    |> resolveOutMsgUpdates updatedModel Cmd.none
                    |> Tuple.mapSecond
                        (\updateCmd ->
                            Cmd.batch
                                [ updateCmd
                                , Cmd.map (GotApiMsg << toMsg) msg
                                ]
                        )
           )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map
            (GotApiMsg << GotGoogleMsg)
            (Google.subscriptions model.api.google)
        , Sub.map
            (GotApiMsg << GotWiktionaryMsg)
            (Wiktionary.subscriptions model.api.wiktionary)
        , case model.page of
            Auth subModel ->
                Sub.none

            Mining subModel ->
                Sub.map GotMiningMsg (Mining.subscriptions subModel)
        ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.page of
        Auth subModel ->
            pageView GotAuthMsg Auth.view subModel

        Mining subModel ->
            pageView GotMiningMsg Mining.view subModel


pageView :
    (subMsg -> Msg)
    -> (subModel -> { title : String, content : Html subMsg })
    -> subModel
    -> Browser.Document Msg
pageView toMsg viewFn subModel =
    let
        { title, content } =
            viewFn subModel
    in
    { title = title
    , body = [ Html.map toMsg content ]
    }



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
