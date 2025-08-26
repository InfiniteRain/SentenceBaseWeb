module Api.Google.Exchange.Drive exposing
    ( ResponseFile
    , ResponseFileCreate
    , ResponseFileList
    , createAppFolderRequest
    , createMainSheetRequest
    , findAppFoldersRequest
    , findMainSheetRequest
    )

import Api.Google.Constants as Constants exposing (MimeType(..), SpecialFile(..))
import Api.Google.Exchange.Task as Task
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Url.Builder exposing (QueryParameter, crossOrigin, string)



-- DECODERS


type alias ResponseFile =
    { kind : String
    , mimeType : String
    , id : String
    , name : String
    }


responseFileDecoder : Decoder ResponseFile
responseFileDecoder =
    Decode.map4 ResponseFile
        (Decode.field "kind" Decode.string)
        (Decode.field "mimeType" Decode.string)
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)


type alias ResponseFileList =
    { files : List ResponseFile
    }


responseFileListDecoder : Decoder ResponseFileList
responseFileListDecoder =
    Decode.map ResponseFileList
        (Decode.field "files" (Decode.list responseFileDecoder))


type alias ResponseFileCreate =
    { id : String }


responseFileCreateDecoder : Decoder ResponseFileCreate
responseFileCreateDecoder =
    Decode.map ResponseFileCreate
        (Decode.field "id" Decode.string)



-- ENCODERS


type alias RequestFile =
    { name : String
    , mimeType : String
    , parents : Maybe (List String)
    }


requestFileEncoder : RequestFile -> Encode.Value
requestFileEncoder file =
    Encode.object <|
        List.concat
            [ [ ( "mimeType", Encode.string file.mimeType )
              , ( "name", Encode.string file.name )
              ]
            , file.parents
                |> Maybe.map
                    (\parents ->
                        [ ( "parents", Encode.list Encode.string parents ) ]
                    )
                |> Maybe.withDefault []
            ]



-- API


findAppFoldersRequest : Task.DriveTask ResponseFileList
findAppFoldersRequest =
    Task.driveHttp
        { method = "GET"
        , url =
            googleApiUrl
                (googleDriveRoute [ "files" ])
                [ string "q"
                    ("name = '"
                        ++ Constants.specialFileName AppFolder
                        ++ "' and mimeType = '"
                        ++ Constants.mimeTypeName Folder
                        ++ "'"
                    )
                , string "orderBy" "createdTime"
                ]
        , body = Http.emptyBody
        , resolver = Task.Json responseFileListDecoder
        }


createAppFolderRequest : Task.DriveTask ResponseFileCreate
createAppFolderRequest =
    Task.driveHttp
        { method = "POST"
        , url = googleApiUrl (googleDriveRoute [ "files" ]) []
        , body =
            Http.jsonBody <|
                requestFileEncoder
                    { name = Constants.specialFileName AppFolder
                    , mimeType = Constants.mimeTypeName Folder
                    , parents = Nothing
                    }
        , resolver = Task.Json responseFileCreateDecoder
        }


findMainSheetRequest : String -> Task.DriveTask ResponseFileList
findMainSheetRequest appFolderId =
    Task.driveHttp
        { method = "GET"
        , url =
            googleApiUrl
                (googleDriveRoute [ "files" ])
                [ string "q"
                    ("name = '"
                        ++ Constants.specialFileName MainSheet
                        ++ "' and mimeType = '"
                        ++ Constants.mimeTypeName SpreadSheet
                        ++ "' and '"
                        ++ appFolderId
                        ++ "' in parents"
                    )
                , string "orderBy" "createdTime"
                ]
        , body = Http.emptyBody
        , resolver = Task.Json responseFileListDecoder
        }


createMainSheetRequest : String -> Task.DriveTask ResponseFileCreate
createMainSheetRequest appFolderId =
    Task.driveHttp
        { method = "POST"
        , url = googleApiUrl (googleDriveRoute [ "files" ]) []
        , body =
            Http.jsonBody
                (requestFileEncoder
                    { name = Constants.specialFileName MainSheet
                    , mimeType = Constants.mimeTypeName SpreadSheet
                    , parents = Just [ appFolderId ]
                    }
                )
        , resolver = Task.Json responseFileCreateDecoder
        }



-- BUILDERS


googleApiUrl : List String -> List QueryParameter -> String
googleApiUrl segments params =
    crossOrigin "https://www.googleapis.com" segments params


googleDriveRoute : List String -> List String
googleDriveRoute additional =
    [ "drive", "v3" ] ++ additional
