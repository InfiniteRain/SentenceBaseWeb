module Api.Google.Migration.M15052025SentencesAndWords exposing
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
import Task exposing (Task)



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
    { id = "SentencesAndWords"
    , model =
        { token = token
        , sheetId = sheetId
        , state = LookingForQuerySheet
        }
    , initialTask =
        TaskCmd.attempt GotCreateSubSheetResponse <|
            createSubSheetsTask token sheetId
    }



-- UPDATE


type Msg
    = GotCreateSubSheetResponse (Result Http.Error ())


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotCreateSubSheetResponse (Err err) ->
            ( model, Effect.fail err )

        GotCreateSubSheetResponse (Ok _) ->
            ( model, Effect.done )


createSubSheetsTask : String -> String -> Task Http.Error ()
createSubSheetsTask token sheetId =
    Requests.sheetBatchUpdateRequest
        token
        sheetId
        (Requests.addSubSheetRequests columnSize
            [ { id = 200
              , name = "pending_sentences"
              , columns =
                    [ ( "word", Word )
                    , ( "sentence", Sentence )
                    , ( "added_at", DateTime )
                    ]
              }
            , { id = 300
              , name = "mined_sentences"
              , columns =
                    [ ( "word", Word )
                    , ( "sentence", Sentence )
                    , ( "batch_id", Id )
                    , ( "mined_at", DateTime )
                    ]
              }
            , { id = 400
              , name = "mined_words"
              , columns =
                    [ ( "word", Word )
                    , ( "mined_at", DateTime )
                    ]
              }
            , { id = 500
              , name = "backlog_sentences"
              , columns =
                    [ ( "word", Word )
                    , ( "sentence", Sentence )
                    , ( "backlogged_at", DateTime )
                    ]
              }
            ]
        )



-- CONSTANTS


type Column
    = DateTime
    | Word
    | Sentence
    | Id


columnSize : Column -> Int
columnSize column =
    case column of
        DateTime ->
            200

        Word ->
            100

        Sentence ->
            500

        Id ->
            75
