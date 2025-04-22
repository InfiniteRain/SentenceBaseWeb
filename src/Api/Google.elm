module Api.Google exposing
    ( Action(..)
    , Error(..)
    , GoogleFileCreateResponse
    , GoogleFileListResponse
    , GoogleRequestConfig
    , decodeGoogleExpect
    , googleCreateAppFolderRequest
    , googleFindAppFoldersRequest
    , googleGetAppFolderId
    , mapAction
    )

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Url.Builder exposing (QueryParameter, crossOrigin, string)



-- ERROR


type Error
    = HttpError Http.Error
    | DecodeError Decode.Error



-- ACTION


type Action msg
    = None
    | Google (GoogleRequestConfig msg)


mapAction : (a -> msg) -> Action a -> Action msg
mapAction map msg =
    case msg of
        Google body ->
            Google
                { method = body.method
                , url = body.url
                , body = body.body
                , expect = mapGoogleExpect map body.expect
                }

        None ->
            None



-- EXTERNAL


type GoogleExpect msg
    = GetAppFolder (Decoder GoogleFileListResponse) (Result Error (Maybe String) -> msg)
    | CreateAppFolder msg


type alias GoogleRequestConfig msg =
    { method : String
    , url : String
    , body : Http.Body
    , expect : GoogleExpect msg
    }


mapGoogleExpect : (a -> expect) -> GoogleExpect a -> GoogleExpect expect
mapGoogleExpect map expect =
    case expect of
        GetAppFolder decoder msgMap ->
            GetAppFolder decoder (\r -> msgMap r |> map)

        CreateAppFolder msg ->
            CreateAppFolder (map msg)


decodeGoogleExpect : GoogleExpect msg -> Result Http.Error String -> msg
decodeGoogleExpect expect result =
    let
        mappedResult =
            Result.mapError HttpError result
    in
    case expect of
        GetAppFolder decoder map ->
            decodeGoogleMappedResult mappedResult decoder
                |> Result.map
                    (\response ->
                        List.head response.files
                            |> Maybe.map (\file -> file.id)
                    )
                |> map

        CreateAppFolder msg ->
            msg


decodeGoogleMappedResult : Result Error String -> Decoder a -> Result Error a
decodeGoogleMappedResult mappedResult decoder =
    mappedResult
        |> Result.andThen
            (\str ->
                Decode.decodeString decoder str
                    |> Result.mapError DecodeError
            )


googleGetAppFolderId : (Result Error (Maybe String) -> msg) -> GoogleRequestConfig msg
googleGetAppFolderId msg =
    { method = "GET"
    , url =
        googleUrl
            (googleDriveRoute [ "files" ])
            [ string "q"
                ("name = '"
                    ++ appFolderName
                    ++ "' and mimeType = '"
                    ++ googleMimeTypes.folder
                    ++ "'"
                )
            ]
    , body = Http.emptyBody
    , expect = GetAppFolder googleFileListResponseDecoder msg
    }



-- INTERNAL


googleFindAppFoldersRequest : String -> (Result Http.Error GoogleFileListResponse -> msg) -> Cmd msg
googleFindAppFoldersRequest token msg =
    httpRequest
        { token = token
        , method = "GET"
        , url =
            googleUrl
                (googleDriveRoute [ "files" ])
                [ string "q"
                    ("name = '"
                        ++ appFolderName
                        ++ "' and mimeType = '"
                        ++ googleMimeTypes.folder
                        ++ "'"
                    )
                , string "orderBy" "createdTime"
                ]
        , body = Http.emptyBody
        , expect = Http.expectJson msg googleFileListResponseDecoder
        }


googleCreateAppFolderRequest : String -> (Result Http.Error GoogleFileCreateResponse -> msg) -> Cmd msg
googleCreateAppFolderRequest token msg =
    httpRequest
        { token = token
        , method = "POST"
        , url = googleUrl (googleDriveRoute [ "files" ]) []
        , body =
            Http.jsonBody
                (googleFileCreateEncoder
                    { name = appFolderName
                    , mimeType = googleMimeTypes.folder
                    }
                )
        , expect = Http.expectJson msg googleFileCreateDecoder
        }



-- DECODERS


type alias GoogleFile =
    { kind : String
    , mimeType : String
    , id : String
    , name : String
    }


googleFileDecoder : Decoder GoogleFile
googleFileDecoder =
    Decode.map4 GoogleFile
        (Decode.field "kind" Decode.string)
        (Decode.field "mimeType" Decode.string)
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)


type alias GoogleFileListResponse =
    { files : List GoogleFile
    }


googleFileListResponseDecoder : Decoder GoogleFileListResponse
googleFileListResponseDecoder =
    Decode.map GoogleFileListResponse
        (Decode.field "files" (Decode.list googleFileDecoder))


type alias GoogleFileCreateResponse =
    { id : String }


googleFileCreateDecoder : Decoder GoogleFileCreateResponse
googleFileCreateDecoder =
    Decode.map GoogleFileCreateResponse
        (Decode.field "id" Decode.string)



-- ENCODERS


type alias GoogleFileCreate =
    { name : String
    , mimeType : String
    }


googleFileCreateEncoder : GoogleFileCreate -> Encode.Value
googleFileCreateEncoder createFile =
    Encode.object
        [ ( "mimeType", Encode.string createFile.mimeType )
        , ( "name", Encode.string createFile.name )
        ]



-- BUILDERS


googleUrl : List String -> List QueryParameter -> String
googleUrl segments params =
    crossOrigin "https://www.googleapis.com" segments params


googleDriveRoute : List String -> List String
googleDriveRoute additional =
    [ "drive", "v3" ] ++ additional


tokenHeader : String -> Http.Header
tokenHeader token =
    Http.header "Authorization" ("Bearer " ++ token)


httpRequest :
    { token : String
    , method : String
    , url : String
    , body : Http.Body
    , expect : Http.Expect msg
    }
    -> Cmd msg
httpRequest { token, method, url, body, expect } =
    Http.request
        { method = method
        , headers = [ tokenHeader token ]
        , url = url
        , body = body
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }



-- REUSED VALUES


appFolderName : String
appFolderName =
    "SentenceBaseData"


googleMimeTypes : { folder : String }
googleMimeTypes =
    { folder = "application/vnd.google-apps.folder"
    }
