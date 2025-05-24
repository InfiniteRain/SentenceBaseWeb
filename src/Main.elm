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



-- MODEL


type alias Model =
    { googleModel : Google.Model Msg
    , wiktionaryModel : Wiktionary.Model Msg
    , page : Page
    }


type Page
    = Auth Auth.Model
    | Mining Mining.Model


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ navKey =
    let
        ( googleModel, googleCmd ) =
            Google.init ()

        ( wiktionaryModel, wiktionaryCmd ) =
            Wiktionary.init ()

        session =
            Session.create navKey

        ( authModel, authCmd ) =
            Auth.init session
    in
    ( { googleModel = googleModel
      , wiktionaryModel = wiktionaryModel
      , page = Auth authModel
      }
    , Cmd.batch
        [ Cmd.map GotGoogleMsg googleCmd
        , Cmd.map GotWiktionaryMsg wiktionaryCmd
        , Cmd.map GotAuthMsg authCmd
        , Route.navigate navKey Route.Auth
        ]
    )


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
    | GotGoogleMsg (Google.Msg Msg)
    | GotWiktionaryMsg (Wiktionary.Msg Msg)
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

        ( GotGoogleMsg subMsg, _ ) ->
            Google.update subMsg model.googleModel
                |> updateWithApi
                    (\googleModel -> { model | googleModel = googleModel })
                    GotGoogleMsg

        ( GotWiktionaryMsg subMsg, _ ) ->
            Wiktionary.update subMsg model.wiktionaryModel
                |> updateWithApi
                    (\wiktionaryModel ->
                        { model | wiktionaryModel = wiktionaryModel }
                    )
                    GotWiktionaryMsg

        ( GotAuthMsg subMsg, Auth subModel ) ->
            Auth.update subMsg subModel
                |> updateWithPage Auth GotAuthMsg model

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
