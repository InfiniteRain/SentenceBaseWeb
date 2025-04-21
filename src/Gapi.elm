module Gapi exposing
    ( Action(..)
    , Error(..)
    , GapiFileCreateResponse
    , GapiFileListResponse
    , GapiRequestConfig
    , decodeGapiExpect
    , gapiCreateAppFolder
    , gapiFindAppFolders
    , gapiGetAppFolderId
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
    | Gapi (GapiRequestConfig msg)


mapAction : (a -> msg) -> Action a -> Action msg
mapAction map msg =
    case msg of
        Gapi body ->
            Gapi
                { method = body.method
                , url = body.url
                , body = body.body
                , expect = mapGapiExpect map body.expect
                }

        None ->
            None



-- GAPI EXTERNAL


type GapiExpect msg
    = GetAppFolder (Decoder GapiFileListResponse) (Result Error (Maybe String) -> msg)
    | CreateAppFolder msg


type alias GapiRequestConfig msg =
    { method : String
    , url : String
    , body : Http.Body
    , expect : GapiExpect msg
    }


mapGapiExpect : (a -> expect) -> GapiExpect a -> GapiExpect expect
mapGapiExpect map expect =
    case expect of
        GetAppFolder decoder msgMap ->
            GetAppFolder decoder (\r -> msgMap r |> map)

        CreateAppFolder msg ->
            CreateAppFolder (map msg)


decodeGapiExpect : GapiExpect msg -> Result Http.Error String -> msg
decodeGapiExpect expect result =
    let
        mappedResult =
            Result.mapError HttpError result
    in
    case expect of
        GetAppFolder decoder map ->
            decodeGapiMappedResult mappedResult decoder
                |> Result.map
                    (\response ->
                        List.head response.files
                            |> Maybe.map (\file -> file.id)
                    )
                |> map

        CreateAppFolder msg ->
            msg


decodeGapiMappedResult : Result Error String -> Decoder a -> Result Error a
decodeGapiMappedResult mappedResult decoder =
    mappedResult
        |> Result.andThen
            (\str ->
                Decode.decodeString decoder str
                    |> Result.mapError DecodeError
            )


gapiGetAppFolderId : (Result Error (Maybe String) -> msg) -> GapiRequestConfig msg
gapiGetAppFolderId msg =
    { method = "GET"
    , url =
        googleApi
            (googleDriveRoute [ "files" ])
            [ string "q"
                ("name = '"
                    ++ appFolderName
                    ++ "' and mimeType = '"
                    ++ gapiMimeTypes.folder
                    ++ "'"
                )
            ]
    , body = Http.emptyBody
    , expect = GetAppFolder gapiFileListResponseDecoder msg
    }



-- GAPI INTERNAL


gapiFindAppFolders : String -> (Result Http.Error GapiFileListResponse -> msg) -> Cmd msg
gapiFindAppFolders token msg =
    httpRequest
        { token = token
        , method = get
        , url =
            googleApi
                (googleDriveRoute [ "files" ])
                [ string "q"
                    ("name = '"
                        ++ appFolderName
                        ++ "' and mimeType = '"
                        ++ gapiMimeTypes.folder
                        ++ "'"
                    )
                , string "orderBy" "createdTime"
                ]
        , body = Http.emptyBody
        , expect = Http.expectJson msg gapiFileListResponseDecoder
        }


gapiCreateAppFolder : String -> (Result Http.Error GapiFileCreateResponse -> msg) -> Cmd msg
gapiCreateAppFolder token msg =
    httpRequest
        { token = token
        , method = post
        , url = googleApi (googleDriveRoute [ "files" ]) []
        , body =
            Http.jsonBody
                (gapiFileCreateEncoder
                    { name = appFolderName
                    , mimeType = gapiMimeTypes.folder
                    }
                )
        , expect = Http.expectJson msg gapiFileCreateDecoder
        }



-- DECODERS


type alias GapiFile =
    { kind : String
    , mimeType : String
    , id : String
    , name : String
    }


gapiFileDecoder : Decoder GapiFile
gapiFileDecoder =
    Decode.map4 GapiFile
        (Decode.field "kind" Decode.string)
        (Decode.field "mimeType" Decode.string)
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)


type alias GapiFileListResponse =
    { files : List GapiFile
    }


gapiFileListResponseDecoder : Decoder GapiFileListResponse
gapiFileListResponseDecoder =
    Decode.map GapiFileListResponse
        (Decode.field "files" (Decode.list gapiFileDecoder))


type alias GapiFileCreateResponse =
    { id : String }


gapiFileCreateDecoder : Decoder GapiFileCreateResponse
gapiFileCreateDecoder =
    Decode.map GapiFileCreateResponse
        (Decode.field "id" Decode.string)



-- ENCODERS


type alias GapiFileCreate =
    { name : String
    , mimeType : String
    }


gapiFileCreateEncoder : GapiFileCreate -> Encode.Value
gapiFileCreateEncoder createFile =
    Encode.object
        [ ( "mimeType", Encode.string createFile.mimeType )
        , ( "name", Encode.string createFile.name )
        ]



-- BUILDERS


googleApi : List String -> List QueryParameter -> String
googleApi segments params =
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


get : String
get =
    "GET"


post : String
post =
    "POST"


appFolderName : String
appFolderName =
    "SentenceBaseData"


gapiMimeTypes : { folder : String }
gapiMimeTypes =
    { folder = "application/vnd.google-apps.folder"
    }
