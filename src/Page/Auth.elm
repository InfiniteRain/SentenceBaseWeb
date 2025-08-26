module Page.Auth exposing (Model, Msg(..), init, subscriptions, update, view)

import Api.Google as Google
    exposing
        ( InitializeFailure(..)
        , InitializeUpdate(..)
        )
import Api.Google.Exchange.Task as Task
import Effect exposing (Effect)
import Html exposing (Html, br, button, div, text)
import Html.Events exposing (onClick)
import Route
import Session exposing (Session)



-- MODEL


type alias Model =
    { statusText : String
    , initFailed : Bool
    , session : Session
    }


init : Session -> ( Model, Cmd Msg, Effect Msg )
init session =
    ( { statusText = "Authenticating..."
      , initFailed = False
      , session = session
      }
    , Cmd.none
    , Effect.googleInitialize InitializeUpdated
    )



-- UPDATE


type Msg
    = InitializeUpdated Google.InitializeUpdate
    | TryAgainClicked


update : Msg -> Model -> ( Model, Cmd Msg, Effect Msg )
update msg ({ session } as model) =
    case msg of
        InitializeUpdated Done ->
            ( { model | session = Session.authenticate session }
            , Route.navigate session Route.Mining
            , Effect.none
            )

        InitializeUpdated (Failed _) ->
            ( { model | initFailed = True }
            , Cmd.none
            , Effect.none
            )

        InitializeUpdated initializeUpdate ->
            ( { model
                | statusText = initializeUpdateToStatusText initializeUpdate
              }
            , Cmd.none
            , Effect.none
            )

        TryAgainClicked ->
            ( { model | initFailed = False }
            , Cmd.none
            , Effect.googleInitialize InitializeUpdated
            )


initializeUpdateToStatusText : Google.InitializeUpdate -> String
initializeUpdateToStatusText initializeUpdate =
    case initializeUpdate of
        AuthenticatingApi ->
            "Authenticating Google API"

        LocatingAppFolder ->
            "Locating the application folder"

        CreatingAppFolder ->
            "Creating the application folder"

        LocatingMainSheet ->
            "Locating main spreadsheet"

        CreatingMainSheet ->
            "Creating main spreadsheet"

        CheckingMainSheetMigrations ->
            "Checking applied migrations"

        MigratingMainSheet migrationId ->
            "Applying migration " ++ migrationId

        Done ->
            "Done"

        Failed failure ->
            case failure of
                ApiAuthentication err ->
                    "Failed to authenticate Google API: "
                        ++ Task.taskPortErrorToMessage err

                AppFolderLocation err ->
                    "Failed to locate the application folder: "
                        ++ Task.errorToMessage err

                AppFolderCreation err ->
                    "Failed to create the application folder"
                        ++ Task.errorToMessage err

                MainSheetLocation err ->
                    "Failed to locate main spreadsheet"
                        ++ Task.errorToMessage err

                MainSheetCreation err ->
                    "Failed to create main spreadsheet"
                        ++ Task.errorToMessage err

                MainSheetMigration migrationId err ->
                    "Failure during migration "
                        ++ migrationId
                        ++ ": "
                        ++ Task.errorToMessage err



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Authentication"
    , content =
        div [] <|
            List.concat
                [ [ text model.statusText
                  ]
                , if model.initFailed then
                    [ br [] []
                    , button [ onClick TryAgainClicked ] [ text "Try again" ]
                    ]

                  else
                    []
                ]
    }
