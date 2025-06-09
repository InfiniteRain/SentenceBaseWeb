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
import Task exposing (Task)



-- MODEL


type alias Model =
    { sheetId : String
    , state : State
    }


type State
    = LookingForQuerySheet


type alias Config =
    Config_.Config Model Msg


init : String -> Config
init sheetId =
    { id = "SentencesAndWords"
    , model =
        { sheetId = sheetId
        , state = LookingForQuerySheet
        }
    , initialTask =
        createSubSheetsTask sheetId
            |> TaskCmd.attempt GotCreateSubSheetResponse
    }



-- UPDATE


type Msg
    = GotCreateSubSheetResponse (Result Requests.Error ())


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotCreateSubSheetResponse (Err err) ->
            ( model, Effect.fail err )

        GotCreateSubSheetResponse (Ok _) ->
            ( model, Effect.done )


createSubSheetsTask : String -> Task Requests.Error ()
createSubSheetsTask sheetId =
    Requests.sheetBatchUpdateRequest
        (Requests.addSubSheetRequests columnSize
            [ { id = 200
              , name = "pending_sentences"
              , columns =
                    [ ( "word", Word )
                    , ( "sentence", Sentence )
                    , ( "tags", Tags )
                    , ( "added_at", DateTime )
                    ]
              }
            , { id = 300
              , name = "mined_sentences"
              , columns =
                    [ ( "word", Word )
                    , ( "sentence", Sentence )
                    , ( "tags", Tags )
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
                    , ( "tags", Tags )
                    , ( "backlogged_at", DateTime )
                    ]
              }
            ]
        )
        sheetId
        |> Requests.buildTask



-- CONSTANTS


type Column
    = DateTime
    | Word
    | Sentence
    | Tags
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

        Tags ->
            150

        Id ->
            300
