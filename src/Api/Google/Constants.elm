module Api.Google.Constants exposing
    ( MimeType(..)
    , SpecialFile(..)
    , SubSheet(..)
    , mimeTypeName
    , specialFileName
    , subSheetId
    , subSheetName
    )

-- SUB SHEET CONSTANTS


type SubSheet
    = Query
    | Migrations
    | PendingSentences
    | MinedSentences
    | MinedWords
    | BacklogSentences


subSheetId : SubSheet -> Int
subSheetId subSheet =
    case subSheet of
        Query ->
            0

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


subSheetName : SubSheet -> String
subSheetName subSheet =
    case subSheet of
        Query ->
            "query"

        Migrations ->
            "migratons"

        PendingSentences ->
            "pending_sentences"

        MinedSentences ->
            "mined_sentences"

        MinedWords ->
            "mined_words"

        BacklogSentences ->
            "backlog_sentences"



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
