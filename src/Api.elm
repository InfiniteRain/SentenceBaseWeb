module Api exposing
    ( Action(..)
    , Error(..)
    , GapiRequestConfig
    , decodeGapiExpect
    , gapiGetAppFolderId
    , mapAction
    )

import Http
import Json.Decode as Decode exposing (Decoder)
import Url.Builder exposing (QueryParameter, crossOrigin, string)


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



-- GAPI


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


gapiRequest : GapiRequestConfig msg -> Action msg
gapiRequest body =
    Gapi body


gapiGetAppFolderId : (Result Error (Maybe String) -> msg) -> Action msg
gapiGetAppFolderId msg =
    gapiRequest
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



-- URL BUILDERS


googleApi : List String -> List QueryParameter -> String
googleApi segments params =
    crossOrigin "https://www.googleapis.com" segments params


googleDriveRoute : List String -> List String
googleDriveRoute additional =
    [ "drive", "v3" ] ++ additional



-- REUSED VALUES


appFolderName : String
appFolderName =
    "SentenceBaseData"


gapiMimeTypes : { folder : String }
gapiMimeTypes =
    { folder = "application/vnd.google-apps.folder"
    }
