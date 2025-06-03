module Page.Auth exposing (Model, Msg(..), init, subscriptions, update, view)

import Api.Action as Action exposing (Action)
import Api.Google as Google exposing (InitializeFailure(..), InitializeUpdate(..))
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


init : Session -> ( Model, Cmd Msg, Action Msg )
init session =
    ( { statusText = "Authenticating..."
      , initFailed = False
      , session = session
      }
    , Cmd.none
    , Action.googleInitialize InitializeUpdated
    )



-- UPDATE


type Msg
    = InitializeUpdated Google.InitializeUpdate
    | TryAgainClicked


update : Msg -> Model -> ( Model, Cmd Msg, Action Msg )
update msg ({ session } as model) =
    case msg of
        InitializeUpdated Done ->
            ( { model | session = Session.authenticate session }
            , Route.navigate (Session.navKey session) Route.Mining
            , Action.none
            )

        InitializeUpdated (Failed _) ->
            ( { model | initFailed = True }
            , Cmd.none
            , Action.none
            )

        InitializeUpdated initializeUpdate ->
            ( { model
                | statusText = initializeUpdateToStatusText initializeUpdate
              }
            , Cmd.none
            , Action.none
            )

        TryAgainClicked ->
            ( { model | initFailed = False }
            , Cmd.none
            , Action.googleInitialize InitializeUpdated
            )


initializeUpdateToStatusText : Google.InitializeUpdate -> String
initializeUpdateToStatusText initializeUpdate =
    case initializeUpdate of
        InitializingApi ->
            "Initializing Google API"

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

        Failed err ->
            case err of
                ApiInitialization _ ->
                    "Failed to initialize Google API"

                ApiAuthentication _ ->
                    "Failed to authenticate Google API"

                AppFolderLocation _ ->
                    "Failed to locate the application folder"

                AppFolderCreation _ ->
                    "Failed to create the application folder"

                MainSheetLocation _ ->
                    "Failed to locate main spreadsheet"

                MainSheetCreation _ ->
                    "Failed to create main spreadsheet"

                MainSheetMigration migrationId _ ->
                    "Failure during migration " ++ migrationId



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
