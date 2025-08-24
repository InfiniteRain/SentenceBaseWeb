module Main exposing (..)

import Api.Google as Google exposing (Msg(..))
import Api.Google.Constants exposing (SubSheet(..))
import Api.OutMsg as OutMsg exposing (OutMsg)
import Api.Uuid as Uuid
import Api.Wiktionary as Wiktionary exposing (Msg(..))
import Basecoat
    exposing
        ( ariaAtomic
        , ariaCurrent
        , ariaHidden
        , ariaLabel
        , ariaLabelledBy
        , classes
        , dataAlign
        , dataSide
        , dataSideBarInitialized
        , inert
        , role
        )
import Browser
import Browser.Dom as Dom exposing (Viewport)
import Browser.Navigation as Nav
import Effect exposing (Effect)
import Html
    exposing
        ( Html
        , a
        , aside
        , button
        , div
        , h2
        , h3
        , header
        , li
        , main_
        , nav
        , p
        , section
        , span
        , text
        , ul
        )
import Html.Attributes
    exposing
        ( class
        , href
        , id
        , target
        , type_
        )
import Html.Events exposing (onClick)
import Icon.Error exposing (errorIcon)
import Icon.GitHub exposing (gitHubIcon)
import Icon.Info exposing (infoIcon)
import Icon.Menu exposing (menuIcon)
import Icon.Pickaxe exposing (pickaxeIcon)
import Icon.Success exposing (successIcon)
import Icon.WarningCircle exposing (warningCircleIcon)
import Page.Auth as Auth exposing (Msg(..))
import Page.Batches as Batches
import Page.Mining as Mining exposing (Msg(..))
import Port.Timeout as Timeout
import Random
import Route exposing (Route(..), standardizeFragment)
import Session exposing (Session)
import Task
import Time
import Toast
import Triple
import UUID as UuidLib
import Url exposing (Url)



-- API SETUP


type ApiMsg
    = GotGoogleMsg (Google.Msg Msg)
    | GotWiktionaryMsg (Wiktionary.Msg Msg)
    | GotUuidMsg (Uuid.Msg Msg)


type alias ApiModel =
    { google : Google.Model Msg
    , wiktionary : Wiktionary.Model Msg
    , uuid : Uuid.Model
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

        GotUuidMsg subMsg ->
            updateWithApi
                subMsg
                apiModel.uuid
                Uuid.update
                GotUuidMsg
                (\model -> { apiModel | uuid = model })


initApi : InitialSeeds -> ( ApiModel, Cmd Msg )
initApi initialSeeds =
    let
        ( googleModel, googleCmd ) =
            Google.init ()

        ( wiktionaryModel, wiktionaryCmd ) =
            Wiktionary.init ()

        ( uuidModel, uuidCmd ) =
            Uuid.init <|
                UuidLib.Seeds
                    (Random.initialSeed initialSeeds.seed1)
                    (Random.initialSeed initialSeeds.seed2)
                    (Random.initialSeed initialSeeds.seed3)
                    (Random.initialSeed initialSeeds.seed4)
    in
    ( { google = googleModel
      , wiktionary = wiktionaryModel
      , uuid = uuidModel
      }
    , Cmd.batch
        [ initApiCmd GotGoogleMsg googleCmd
        , initApiCmd GotWiktionaryMsg wiktionaryCmd
        , initApiCmd GotUuidMsg uuidCmd
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
        , subscribeApi
            GotUuidMsg
            Uuid.subscriptions
            apiModel.uuid
        ]



-- PAGE SETUP


type PageMsg
    = GotAuthMsg Auth.Msg
    | GotMiningMsg Mining.Msg
    | GotBatchesMsg Batches.Msg


type PageModel
    = Auth Auth.Model
    | Mining Mining.Model
    | Batches Batches.Model


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

        ( GotBatchesMsg subMsg, Batches subModel ) ->
            updateWithPage
                subMsg
                subModel
                Batches.update
                GotBatchesMsg
                Batches

        _ ->
            \model -> ( model, Cmd.none )


routeToPage : Maybe Route -> Session -> ( PageModel, Cmd Msg, Effect Msg )
routeToPage maybeRoute session =
    case maybeRoute of
        Just Route.Root ->
            initAuth session

        Just Route.Mining ->
            initPageWith
                session
                Mining.init
                GotMiningMsg
                Mining

        Just Route.Batches ->
            initPageWith
                session
                Batches.init
                GotBatchesMsg
                Batches

        Nothing ->
            initAuth session


pageToSession : PageModel -> Session
pageToSession page =
    case page of
        Auth { session } ->
            session

        Mining { session } ->
            session

        Batches { session } ->
            session


mapPageSession : Session -> PageModel -> PageModel
mapPageSession session page =
    case page of
        Auth model ->
            Auth { model | session = session }

        Mining model ->
            Mining { model | session = session }

        Batches model ->
            Batches { model | session = session }


initAuth : Session -> ( PageModel, Cmd Msg, Effect Msg )
initAuth session =
    initPageWith
        session
        Auth.init
        GotAuthMsg
        Auth


initPage : Session -> ( PageModel, Cmd Msg, Effect Msg )
initPage session =
    initAuth session


pageSubscriptions : PageModel -> Sub Msg
pageSubscriptions pageModel =
    case pageModel of
        Auth subModel ->
            subscribePage GotAuthMsg Auth.subscriptions subModel

        Mining subModel ->
            subscribePage GotMiningMsg Mining.subscriptions subModel

        Batches subModel ->
            subscribePage
                GotBatchesMsg
                Batches.subscriptions
                subModel


pageView : PageModel -> Browser.Document Msg
pageView pageModel =
    case pageModel of
        Auth subModel ->
            viewWithPage GotAuthMsg Auth.view subModel

        Mining subModel ->
            viewWithPage GotMiningMsg Mining.view subModel

        Batches subModel ->
            viewWithPage GotBatchesMsg Batches.view subModel



-- MODEL


type alias Model =
    { api : ApiModel
    , page : PageModel
    , sideBarToggled : Bool
    , nextToastId : Int
    , toasts : List Toast
    }


type alias Toast =
    { id : Int
    , config : Toast.Config
    , hidden : Bool
    }


init : InitialSeeds -> Url -> Nav.Key -> ( Model, Cmd Msg )
init seeds url navKey =
    let
        ( apiModel, apiCmd ) =
            initApi seeds

        session =
            Session.create navKey url Time.utc

        ( pageModel, pageCmd, pageEffect ) =
            initPage session

        model =
            { api = apiModel
            , page = pageModel
            , sideBarToggled = False
            , nextToastId = 0
            , toasts = []
            }

        ( initialModel, initialPageCmd ) =
            performEffect model pageEffect
    in
    ( initialModel
    , Cmd.batch <|
        List.concat
            [ [ initialPageCmd
              , apiCmd
              , pageCmd
              , Task.perform ZoneFetched Time.here
              ]
            , if Route.fromUrl url /= Just Route.Root then
                [ Route.navigate session Route.Root
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
    -> (Session -> ( model, Cmd msg, Effect msg ))
    -> (msg -> PageMsg)
    -> (model -> PageModel)
    -> ( PageModel, Cmd Msg, Effect Msg )
initPageWith session initFn toMsg toModel =
    initFn session
        |> Triple.mapAll
            toModel
            (Cmd.map (GotPageMsg << toMsg))
            (Effect.map (GotPageMsg << toMsg))



-- UPDATE


type Msg
    = ZoneFetched Time.Zone
    | UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | GotApiMsg ApiMsg
    | GotPageMsg PageMsg
    | ToggledSideBar (Maybe String)
    | BlurredSideBarItem
    | ClickedSideBarItem String
    | GotViewPort String Viewport
    | Timeout Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        session =
            pageToSession model.page
    in
    case ( msg, model.page ) of
        ( ZoneFetched zone, page ) ->
            ( { model
                | page = mapPageSession (Session.replaceZone zone session) page
              }
            , Cmd.none
            )

        ( UrlChanged url, _ ) ->
            routeToPage (Route.fromUrl url) (Session.replaceUrl url session)
                |> Triple.mapFirst (\page -> { model | page = page })
                |> (\( newModel, cmd, effect ) ->
                        effect
                            |> performEffect newModel
                            |> Tuple.mapSecond
                                (\googleEffectCmd ->
                                    Cmd.batch
                                        [ googleEffectCmd
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
                        (Url.toString <| standardizeFragment url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( GotApiMsg apiMsg, _ ) ->
            updateApi apiMsg model.api model

        ( GotPageMsg pageMsg, pageModel ) ->
            updatePage pageMsg pageModel model

        ( ToggledSideBar maybeItemId, _ ) ->
            case maybeItemId of
                Just itemId ->
                    ( model
                    , Task.attempt (\_ -> BlurredSideBarItem) (Dom.blur itemId)
                    )

                Nothing ->
                    ( { model | sideBarToggled = not model.sideBarToggled }
                    , Cmd.none
                    )

        ( BlurredSideBarItem, _ ) ->
            ( { model | sideBarToggled = not model.sideBarToggled }, Cmd.none )

        ( ClickedSideBarItem itemId, _ ) ->
            ( model, Task.perform (GotViewPort itemId) Dom.getViewport )

        ( GotViewPort itemId viewport, _ ) ->
            if viewport.scene.width < mobileSideBarViewportMinWidth then
                update (ToggledSideBar <| Just itemId) model

            else
                ( model, Cmd.none )

        ( Timeout id, _ ) ->
            ( { model
                | toasts =
                    model.toasts
                        |> List.filter
                            (\toast -> toast.id /= id || not toast.hidden)
                        |> List.map
                            (\toast ->
                                if toast.id == id then
                                    { toast | hidden = True }

                                else
                                    toast
                            )
              }
            , Cmd.none
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


performEffect : Model -> Effect Msg -> ( Model, Cmd Msg )
performEffect model effect =
    Effect.match effect
        { onNone = ( model, Cmd.none )
        , onGoogle =
            \action ->
                update
                    (Google.SentAction action
                        |> GotGoogleMsg
                        |> GotApiMsg
                    )
                    model
        , onWiktionary =
            \request ->
                update
                    (Wiktionary.SentRequest request
                        |> GotWiktionaryMsg
                        |> GotApiMsg
                    )
                    model
        , onUuid =
            \toMsg ->
                update
                    (Uuid.SentRequest toMsg
                        |> GotUuidMsg
                        |> GotApiMsg
                    )
                    model
        , onToast =
            \config ->
                let
                    time =
                        if config.category == Toast.Error then
                            5000

                        else
                            3000
                in
                ( { model
                    | nextToastId = model.nextToastId + 1
                    , toasts =
                        model.toasts
                            ++ [ { id = model.nextToastId
                                 , config = config
                                 , hidden = False
                                 }
                               ]
                  }
                , Cmd.batch
                    [ Timeout.set model.nextToastId time
                        |> Task.perform Timeout
                    , Timeout.set model.nextToastId (time + 500)
                        |> Task.perform Timeout
                    ]
                )
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
    -> (msg -> model -> ( model, Cmd msg, Effect msg ))
    -> (msg -> PageMsg)
    -> (model -> PageModel)
    -> Update
updateWithPage subMsg subModel updateFn toMsg toModel model =
    updateFn subMsg subModel
        |> Triple.mapFirst
            (\updatedSubModel ->
                { model | page = toModel updatedSubModel }
            )
        |> (\( newModel, cmd, effect ) ->
                Effect.map (GotPageMsg << toMsg) effect
                    |> performEffect newModel
                    |> Tuple.mapSecond
                        (\googleEffectCmd ->
                            Cmd.batch
                                [ googleEffectCmd
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
    let
        { title, body } =
            pageView model.page

        bodyPage =
            div
                [ classes
                    [ "p-4"
                    , "grow-1"
                    , "shrink-1"
                    , "basis-auto"
                    ]
                ]
                body
    in
    { title = title
    , body =
        List.concat
            [ case model.page of
                Auth _ ->
                    [ bodyPage ]

                _ ->
                    [ sideBarView model
                    , main_
                        [ id "content"
                        , classes
                            [ "h-screen"
                            , "flex"
                            , "flex-col"
                            ]
                        ]
                        [ headerView
                        , bodyPage
                        ]
                    ]
            , [ div [ class "toaster", id "toaster" ] <|
                    List.map
                        (\toast ->
                            div
                                [ class "toast"
                                , role <|
                                    case toast.config.category of
                                        Toast.Error ->
                                            "alert"

                                        _ ->
                                            "status"
                                , ariaAtomic True
                                , ariaHidden toast.hidden
                                ]
                                [ div [ class "toast-content" ]
                                    [ case toast.config.category of
                                        Toast.Success ->
                                            successIcon []

                                        Toast.Error ->
                                            errorIcon []

                                        Toast.Info ->
                                            infoIcon []

                                        Toast.Warning ->
                                            warningCircleIcon []
                                    , section []
                                        [ h2 [] [ text toast.config.title ]
                                        , p [] [ text toast.config.description ]
                                        ]
                                    ]
                                ]
                        )
                        model.toasts
              ]
            ]
    }


sideBarView : Model -> Html Msg
sideBarView model =
    let
        ( miningAttrs, batchesAttrs ) =
            case model.page of
                Auth _ ->
                    ( [], [] )

                Mining _ ->
                    ( [ ariaCurrent "page" ], [] )

                Batches _ ->
                    ( [], [ ariaCurrent "page" ] )

        sideBarAttrs =
            if not model.sideBarToggled then
                [ inert "" ]

            else
                []
    in
    aside
        ([ ariaHidden (not model.sideBarToggled)
         , class "sidebar"
         , dataSide "left"
         , dataSideBarInitialized True
         ]
            ++ sideBarAttrs
        )
        [ nav [ ariaLabel "Sidebar navigation" ]
            [ header []
                [ div
                    [ classes
                        [ "flex"
                        , "p-2"
                        , "h-12"
                        , "w-full"
                        , "justify-start"
                        , "gap-2"
                        ]
                    ]
                    [ div
                        [ classes
                            [ "bg-sidebar-primary"
                            , "text-sidebar-primary-foreground"
                            , "flex"
                            , "aspect-square"
                            , "size-8"
                            , "items-center"
                            , "justify-center"
                            , "rounded-lg"
                            , "p-1"
                            ]
                        ]
                        [ pickaxeIcon [] ]
                    , div
                        [ classes
                            [ "grid"
                            , "flex-1"
                            , "text-left"
                            , "text-sm"
                            , "leading-tight"
                            ]
                        ]
                        [ span [ classes [ "truncate", "font-medium" ] ]
                            [ text "Sentence Base" ]
                        , span
                            [ classes [ "truncate", "text-xs" ] ]
                            [ text "v0.0.1" ]
                        ]
                    ]
                ]
            , section [ class "scrollbar" ]
                [ div [ ariaLabelledBy "group-label-content-1", role "group" ]
                    [ h3 [ id "group-label-content-1" ]
                        [ text "Navigation" ]
                    , ul []
                        [ li []
                            [ a
                                ([ href "#mining"
                                 , id "link-mining"
                                 , onClick (ClickedSideBarItem "link-mining")
                                 ]
                                    ++ miningAttrs
                                )
                                [ span []
                                    [ text "Mining" ]
                                ]
                            ]
                        , li []
                            [ a
                                ([ href "#batches"
                                 , id "link-batches"
                                 , onClick (ClickedSideBarItem "link-batches")
                                 ]
                                    ++ batchesAttrs
                                )
                                [ span []
                                    [ text "Batches" ]
                                ]
                            ]
                        ]
                    ]
                , div [ ariaLabelledBy "group-label-content-2", role "group" ]
                    [ h3 [ id "group-label-content-2" ]
                        [ text "About" ]
                    , ul []
                        [ li []
                            [ a
                                [ href "https://github.com/InfiniteRain/SentenceBaseWeb"
                                , target "_blank"
                                ]
                                [ gitHubIcon []
                                , span [] [ text "GitHub" ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , div [ class "sidebar-backdrop", onClick (ToggledSideBar Nothing) ] []
        ]


headerView : Html Msg
headerView =
    header
        [ classes
            [ "bg-background"
            , "sticky"
            , "inset-x-0"
            , "top-0"
            , "isolate"
            , "flex"
            , "shrink-0"
            , "items-center"
            , "gap-2"
            , "border-b"
            ]
        ]
        [ div
            [ classes
                [ "flex", "h-14", "w-full", "items-center", "gap-2", "px-4" ]
            ]
            [ button
                [ ariaLabel "Toggle sidebar"
                , dataAlign "start"
                , dataSide "bottom"
                , classes
                    [ "btn-sm-icon-ghost"
                    , "mr-auto"
                    , "size-7"
                    , "-ml-1.5"
                    ]
                , type_ "button"
                , onClick (ToggledSideBar Nothing)
                ]
                [ menuIcon [] ]
            ]
        ]


viewWithPage :
    (subMsg -> PageMsg)
    -> (subModel -> { title : String, content : Html subMsg })
    -> subModel
    -> Browser.Document Msg
viewWithPage toMsg viewFn subModel =
    let
        { title, content } =
            viewFn subModel
    in
    { title = title
    , body = [ Html.map (GotPageMsg << toMsg) content ]
    }


mobileSideBarViewportMinWidth : Float
mobileSideBarViewportMinWidth =
    768



-- MAIN


type alias InitialSeeds =
    { seed1 : Int
    , seed2 : Int
    , seed3 : Int
    , seed4 : Int
    }


main : Program InitialSeeds Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
