module Api.Google.Migration.M15052025SentencesAndWords exposing
    ( Config
    , Model
    , Msg
    , init
    , update
    )

import Api.Google.Constants exposing (Column(..), SubSheet(..))
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
        (List.concat
            [ Requests.addTableBatchUpdateRequests
                { kind = PendingSentences
                , columns =
                    [ { kind = Word, name = "word" }
                    , { kind = Sentence, name = "sentence" }
                    , { kind = DateTime, name = "added_at" }
                    ]
                }
            , Requests.addTableBatchUpdateRequests
                { kind = MinedSentences
                , columns =
                    [ { kind = Word, name = "word" }
                    , { kind = Sentence, name = "sentence" }
                    , { kind = Id, name = "batch_id" }
                    , { kind = DateTime, name = "mined_at" }
                    ]
                }
            , Requests.addTableBatchUpdateRequests
                { kind = MinedWords
                , columns =
                    [ { kind = Word, name = "word" }
                    , { kind = DateTime, name = "mined_at" }
                    ]
                }
            , Requests.addTableBatchUpdateRequests
                { kind = BacklogSentences
                , columns =
                    [ { kind = Word, name = "word" }
                    , { kind = Sentence, name = "sentence" }
                    , { kind = DateTime, name = "backlogged_at" }
                    ]
                }
            ]
        )
