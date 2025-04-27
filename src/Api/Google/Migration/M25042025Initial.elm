module Api.Google.Migration.M25042025Initial exposing
    ( Config
    , Model
    , Msg
    , init
    , update
    )

import Api.Google.Migration.Config as Config_
import Api.Google.Migration.Effect as Effect exposing (Effect)
import Api.Google.Requests as Requests
import Api.Google.TaskCmd as TaskCmd
import Http



-- MODEL


type alias Model =
    { token : String
    , sheetId : String
    , state : State
    }


type State
    = LookingForQuerySheet


type alias Config =
    Config_.Config Model Msg


init : String -> String -> Config
init token sheetId =
    { id = "Initial"
    , model =
        { token = token
        , sheetId = sheetId
        , state = LookingForQuerySheet
        }
    , initialTask =
        TaskCmd.attempt Response <| Requests.getSubSheetData token sheetId []
    }



-- UPDATE


type Msg
    = Response (Result Http.Error Requests.SheetResponseGetSubSheetData)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    ( model, Effect.done )
