module Api.Google.Migration.M15052025SentencesAndWords exposing
    ( Config
    , Model
    , Msg
    , init
    , update
    )

import Api.Google.Exchange.Sheets as Sheets
import Api.Google.Exchange.Task as Task
import Api.Google.Migration.Config as Config_
import Api.Google.Migration.Effect as Effect exposing (Effect)



-- MODEL


type alias Model =
    ()


type alias Config =
    Config_.Config Model Msg


init : Config
init =
    { id = "SentencesAndWords"
    , model = ()
    , initialSheetsCmd =
        createSubSheetsTask
            |> Task.sheetsAttempt GotCreateSubSheetResponse
    }



-- UPDATE


type Msg
    = GotCreateSubSheetResponse (Result Task.Error ())


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotCreateSubSheetResponse (Err err) ->
            ( model, Effect.fail err )

        GotCreateSubSheetResponse (Ok _) ->
            ( model, Effect.done )


createSubSheetsTask : Task.SheetsTask ()
createSubSheetsTask =
    Sheets.batchUpdateRequest
        (Sheets.addSubSheetRequests columnSize
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
