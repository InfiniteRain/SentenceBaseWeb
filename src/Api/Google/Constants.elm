module Api.Google.Constants exposing
    ( Column(..)
    , MimeType(..)
    , SpecialFile(..)
    , SubSheet(..)
    , columnSize
    , mimeTypeName
    , specialFileName
    , subSheetId
    , subSheetName
    )

-- COLUMN CONSTANTS


type Column
    = MigrationName
    | DateTime
    | Word
    | Sentence
    | Id


columnSize : Column -> Int
columnSize column =
    case column of
        MigrationName ->
            150

        DateTime ->
            200

        Word ->
            100

        Sentence ->
            500

        Id ->
            75



-- SUB SHEET CONSTANTS


type SubSheet
    = Migrations
    | PendingSentences
    | MinedSentences
    | MinedWords
    | BacklogSentences


subSheetName : SubSheet -> String
subSheetName subSheet =
    case subSheet of
        Migrations ->
            "migrations"

        PendingSentences ->
            "pending_sentences"

        MinedSentences ->
            "mined_sentences"

        MinedWords ->
            "mined_words"

        BacklogSentences ->
            "backlog_sentences"


subSheetId : SubSheet -> Int
subSheetId subSheet =
    case subSheet of
        Migrations ->
            100

        PendingSentences ->
            200

        MinedSentences ->
            300

        MinedWords ->
            400

        BacklogSentences ->
            500



-- SPECIAL FILE CONSTANTS


type SpecialFile
    = AppFolder
    | MainSheet


specialFileName : SpecialFile -> String
specialFileName specialFile =
    case specialFile of
        AppFolder ->
            "SentenceBaseData"

        MainSheet ->
            "MainSheet"



-- MIME TYPE CONSTANTS


type MimeType
    = Folder
    | SpreadSheet


mimeTypeName : MimeType -> String
mimeTypeName mimeType =
    case mimeType of
        Folder ->
            "application/vnd.google-apps.folder"

        SpreadSheet ->
            "application/vnd.google-apps.spreadsheet"
