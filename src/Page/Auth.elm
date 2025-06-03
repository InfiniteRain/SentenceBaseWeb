module Page.Auth exposing (..)

import Api.Action as Action exposing (Action)
import Api.Google as Google exposing (InitializeFailure(..), InitializeUpdate(..))
import Html exposing (Html, div, text)
import Route
import Session exposing (Session)



-- MODEL


type alias Model =
    { statusText : String
    , session : Session
    }


init : Session -> ( Model, Cmd Msg, Action Msg )
init session =
    ( { statusText = "Authenticating..."
      , session = session
      }
    , Cmd.none
    , Action.googleInitialize InitializeUpdated
    )



-- UPDATE


type Msg
    = InitializeUpdated Google.InitializeUpdate


update : Msg -> Model -> ( Model, Cmd Msg, Action Msg )
update (InitializeUpdated initializeUpdate) ({ session } as model) =
    case initializeUpdate of
        Google.Done ->
            ( { model | session = Session.authenticate session }
            , Route.navigate (Session.navKey session) Route.Mining
            , Action.none
            )

        _ ->
            ( { model
                | statusText = initializeUpdateToStatusText initializeUpdate
              }
            , Cmd.none
            , Action.none
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



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Authentication"
    , content =
        div []
            [ text model.statusText
            ]
    }
