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


updateApi : ApiMsg -> ApiModel -> Update
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


apiSubscriptions : ApiModel -> Sub Msg
apiSubscriptions apiModel =
    Sub.batch
        [ subscribeApi
            GotGoogleMsg
            Google.subscriptions
            apiModel.google
        , subscribeApi
            GotWiktionaryMsg
            Wiktionary.subscriptions
            apiModel.wiktionary
        ]



-- PAGE SETUP


type PageMsg
    = GotAuthMsg Auth.Msg
    | GotMiningMsg Mining.Msg


type PageModel
    = Auth Auth.Model
    | Mining Mining.Model


updatePage : PageMsg -> PageModel -> Update
updatePage pageMsg pageModel =
    case ( pageMsg, pageModel ) of
        ( GotAuthMsg subMsg, Auth subModel ) ->
            updateWithPage
                subMsg
                subModel
                Auth.update
                GotAuthMsg
                Auth

        ( GotMiningMsg subMsg, Mining subModel ) ->
            updateWithPage
                subMsg
                subModel
                Mining.update
                GotMiningMsg
                Mining

        _ ->
            let
                _ =
                    Debug.log
                        "invalid page update combination"
                        ( pageMsg, pageModel )
            in
            \model -> ( model, Cmd.none )


routeToPage : Maybe Route -> Session -> ( PageModel, Cmd Msg, Action Msg )
routeToPage maybeRoute session =
    case maybeRoute of
        Just Route.Auth ->
            initAuth session

        Just Route.Mining ->
            initPageWith
                session
                Mining.init
                GotMiningMsg
                Mining

        Nothing ->
            initAuth session


pageToSession : PageModel -> Session
pageToSession page =
    case page of
        Auth model ->
            model.session

        Mining model ->
            model.session


initAuth : Session -> ( PageModel, Cmd Msg, Action Msg )
initAuth session =
    initPageWith
        session
        Auth.init
        GotAuthMsg
        Auth


initPage : Session -> ( PageModel, Cmd Msg, Action Msg )
initPage session =
    initAuth session


pageSubscriptions : PageModel -> Sub Msg
pageSubscriptions pageModel =
    case pageModel of
        Auth subModel ->
            subscribePage GotAuthMsg Auth.subscriptions subModel

        Mining subModel ->
            subscribePage GotMiningMsg Mining.subscriptions subModel



-- MODEL


type alias Model =
    { api : ApiModel
    , page : PageModel
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    let
        ( apiModel, apiCmd ) =
            initApi

        session =
            Session.create navKey

        ( pageModel, pageCmd, pageAction ) =
            initPage session

        model =
            { api = apiModel
            , page = pageModel
            }

        ( initialModel, initialPageCmd ) =
            performApiAction model pageAction
    in
    ( initialModel
    , Cmd.batch <|
        List.concat
            [ [ initialPageCmd
              , apiCmd
              , pageCmd
              ]
            , if Route.fromUrl url /= Just Route.Auth then
                [ Route.navigate navKey Route.Auth
                ]

              else
                []
            ]
    )


initApiCmd : (msg -> ApiMsg) -> Cmd msg -> Cmd Msg
initApiCmd toApiMsg cmd =
    Cmd.map (GotApiMsg << toApiMsg) cmd


initPageWith :
    Session
    -> (Session -> ( model, Cmd msg, Action msg ))
    -> (msg -> PageMsg)
    -> (model -> PageModel)
    -> ( PageModel, Cmd Msg, Action Msg )
initPageWith session initFn toMsg toModel =
    initFn session
        |> Triple.mapAll
            toModel
            (Cmd.map (GotPageMsg << toMsg))
            (Action.map (GotPageMsg << toMsg))



-- UPDATE


type Msg
    = UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | GotApiMsg ApiMsg
    | GotPageMsg PageMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        session =
            pageToSession model.page
    in
    case ( msg, model.page ) of
        ( UrlChanged url, _ ) ->
            routeToPage (Route.fromUrl url) session
                |> Triple.mapFirst (\page -> { model | page = page })
                |> (\( newModel, cmd, action ) ->
                        action
                            |> performApiAction newModel
                            |> Tuple.mapSecond
                                (\googleActionCmd ->
                                    Cmd.batch
                                        [ googleActionCmd
                                        , cmd
                                        ]
                                )
                   )

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

        ( GotPageMsg pageMsg, pageModel ) ->
            updatePage pageMsg pageModel model


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


type alias Update =
    Model -> ( Model, Cmd Msg )


updateWithApi :
    msg
    -> model
    -> (msg -> model -> ( model, Cmd msg, OutMsg Msg ))
    -> (msg -> ApiMsg)
    -> (model -> ApiModel)
    -> Update
updateWithApi subMsg subModel updateFn toMsg toModel model =
    updateFn subMsg subModel
        |> Triple.mapFirst
            (\newSubModel -> { model | api = toModel newSubModel })
        |> (\( newModel, msg, outMsg ) ->
                outMsg
                    |> OutMsg.toList
                    |> resolveOutMsgUpdates newModel Cmd.none
                    |> Tuple.mapSecond
                        (\updateCmd ->
                            Cmd.batch
                                [ updateCmd
                                , Cmd.map (GotApiMsg << toMsg) msg
                                ]
                        )
           )


updateWithPage :
    msg
    -> model
    -> (msg -> model -> ( model, Cmd msg, Action msg ))
    -> (msg -> PageMsg)
    -> (model -> PageModel)
    -> Update
updateWithPage subMsg subModel updateFn toMsg toModel model =
    updateFn subMsg subModel
        |> Triple.mapFirst
            (\updatedSubModel ->
                { model | page = toModel updatedSubModel }
            )
        |> (\( newModel, cmd, action ) ->
                Action.map (GotPageMsg << toMsg) action
                    |> performApiAction newModel
                    |> Tuple.mapSecond
                        (\googleActionCmd ->
                            Cmd.batch
                                [ googleActionCmd
                                , Cmd.map (GotPageMsg << toMsg) cmd
                                ]
                        )
           )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ apiSubscriptions model.api
        , pageSubscriptions model.page
        ]


subscribeApi : (msg -> ApiMsg) -> (model -> Sub msg) -> model -> Sub Msg
subscribeApi toMsg subs subModel =
    Sub.map
        (GotApiMsg << toMsg)
        (subs subModel)


subscribePage : (msg -> PageMsg) -> (model -> Sub msg) -> model -> Sub Msg
subscribePage toMsg subs subModel =
    Sub.map
        (GotPageMsg << toMsg)
        (subs subModel)



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.page of
        Auth subModel ->
            pageView (GotPageMsg << GotAuthMsg) Auth.view subModel

        Mining subModel ->
            pageView (GotPageMsg << GotMiningMsg) Mining.view subModel


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
