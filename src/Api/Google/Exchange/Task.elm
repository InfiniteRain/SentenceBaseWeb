module Api.Google.Exchange.Task exposing
    ( DriveTask
    , Error(..)
    , Resolver(..)
    , SheetsTask
    , Task
    , andThen
    , driveAttempt
    , driveHttp
    , errorToMessage
    , map
    , platform
    , sheetsAttempt
    , sheetsHttp
    , succeed
    )

import Api.Google.Exchange.SheetsCmd as SheetCmd exposing (SheetsCmd)
import Http exposing (Error(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode exposing (encode)
import Port
import Task
import TaskPort exposing (Error(..))



-- TYPES


type Error
    = HttpError Http.Error
    | TokenAcquisitionError TaskPort.Error
    | PlatformTaskError


type Resolver a
    = Json (Decoder a)
    | Custom a


type Task param a
    = ExchangeTask (String -> param -> Task.Task Error a)
    | PlatformTask (Task.Task Error a)


type alias DriveTask a =
    Task () a


type alias SheetsTask a =
    Task String a



-- CONSTRUCTORS


driveHttp :
    { method : String
    , url : String
    , body : Http.Body
    , resolver : Resolver a
    }
    -> DriveTask a
driveHttp { method, url, body, resolver } =
    ExchangeTask <|
        \token () ->
            Http.task
                { method = method
                , headers = [ tokenHeader token ]
                , url = url
                , body = body
                , resolver =
                    case resolver of
                        Json decoder ->
                            jsonResolver decoder

                        Custom value ->
                            jsonResolver <| Decode.succeed value
                , timeout = Nothing
                }


sheetsHttp :
    (String
     ->
        { method : String
        , url : String
        , body : Http.Body
        , resolver : Resolver a
        }
    )
    -> Task String a
sheetsHttp constr =
    ExchangeTask <|
        \token sheetId ->
            let
                { method, url, body, resolver } =
                    constr sheetId
            in
            Http.task
                { method = method
                , headers = [ tokenHeader token ]
                , url = url
                , body = body
                , resolver =
                    case resolver of
                        Json decoder ->
                            jsonResolver decoder

                        Custom value ->
                            jsonResolver <| Decode.succeed value
                , timeout = Nothing
                }


platform : Task.Task error a -> Task param a
platform platformTask =
    platformTask
        |> Task.mapError (\_ -> PlatformTaskError)
        |> PlatformTask


succeed : a -> Task param a
succeed value =
    PlatformTask <| Task.succeed value



-- TRANSFORMERS


map : (a -> b) -> Task param a -> Task param b
map fn task =
    case task of
        ExchangeTask t ->
            ExchangeTask
                (\token sheetId ->
                    t token sheetId
                        |> Task.map fn
                )

        PlatformTask t ->
            PlatformTask <| Task.map fn t


andThen : (a -> Task param b) -> Task param a -> Task param b
andThen fn task =
    ExchangeTask <|
        case task of
            ExchangeTask taskConstr ->
                \token sheetId ->
                    taskConstr token sheetId
                        |> Task.andThen (unwrap sheetId << fn)

            PlatformTask platformTask ->
                \_ sheetId ->
                    platformTask
                        |> Task.andThen (unwrap sheetId << fn)


sheetsAttempt :
    (Result Error a -> msg)
    -> Task String a
    -> SheetsCmd msg
sheetsAttempt resultToMsg task =
    SheetCmd.wrap
        (\sheetId ->
            unwrap sheetId task
                |> Task.attempt resultToMsg
        )


driveAttempt : (Result Error a -> msg) -> DriveTask a -> Cmd msg
driveAttempt resultToMsg task =
    unwrap () task
        |> Task.attempt resultToMsg



-- HELPERS


unwrap : param -> Task param a -> Task.Task Error a
unwrap param task =
    case task of
        ExchangeTask taskConstr ->
            Port.googleGetToken
                |> Task.mapError TokenAcquisitionError
                |> Task.andThen (\token -> taskConstr token param)
                |> Task.onError
                    (\err ->
                        case err of
                            HttpError (BadStatus 401) ->
                                Port.googleGetTokenRefresh
                                    |> Task.mapError TokenAcquisitionError
                                    |> Task.andThen
                                        (\token -> taskConstr token param)

                            _ ->
                                Task.fail err
                    )

        PlatformTask platformTask ->
            platformTask


tokenHeader : String -> Http.Header
tokenHeader token =
    Http.header "Authorization" ("Bearer " ++ token)


handleJsonResponse : Decoder a -> Http.Response String -> Result Error a
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err <| HttpError <| Http.BadUrl url

        Http.Timeout_ ->
            Err <| HttpError <| Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err <| HttpError <| Http.BadStatus statusCode

        Http.NetworkError_ ->
            Err <| HttpError <| Http.NetworkError

        Http.GoodStatus_ _ body ->
            case Decode.decodeString decoder body of
                Err _ ->
                    Err <| HttpError <| Http.BadBody body

                Ok result ->
                    Ok result


jsonResolver : Decoder response -> Http.Resolver Error response
jsonResolver decoder =
    Http.stringResolver <| handleJsonResponse <| decoder


errorToMessage : Error -> String
errorToMessage error =
    case error of
        HttpError httpError ->
            case httpError of
                BadUrl url ->
                    "Bad url: '" ++ url ++ "'."

                Timeout ->
                    "Request timed out."

                NetworkError ->
                    "Encountered a network error."

                BadStatus status ->
                    "Bad response status code: " ++ String.fromInt status ++ "."

                BadBody info ->
                    "Received invalid body: " ++ info

        TokenAcquisitionError taError ->
            case taError of
                InteropError _ ->
                    "Interop error."

                JSError (TaskPort.ErrorObject name _) ->
                    "Error thrown: " ++ name

                JSError (TaskPort.ErrorValue value) ->
                    "Error thrown: " ++ encode 4 value

        PlatformTaskError ->
            "Platform task error."
