module Effect exposing
    ( Definition
    , Definitions(..)
    , Effect
    , Example
    , FormUsages
    , GoogleAction(..)
    , InitializeFailure(..)
    , InitializeUpdate(..)
    , ToastAction
    , ToastCategory(..)
    , Usages(..)
    , UuidAction(..)
    , google
    , googleInitialize
    , map
    , match
    , none
    , toast
    , uuid
    , uuids
    , wiktionary
    )

import Api.Google.Exchange.SheetsCmd as SheetsCmd exposing (SheetsCmd)
import Api.Google.Exchange.Task as Task
import Http
import TaskPort



-- TYPES


type Effect msg
    = Effect
        { action : Action msg
        }


type Action msg
    = None
    | Google (GoogleAction msg)
    | Wiktionary (WiktionaryAction msg)
    | Uuid (UuidAction msg)
    | Toast ToastAction



-- CONSTRUCTORS


none : Effect msg
none =
    Effect { action = None }


google : SheetsCmd msg -> Effect msg
google sheetsCmd =
    Effect { action = Google <| SendRequest sheetsCmd }


googleInitialize : (InitializeUpdate -> msg) -> Effect msg
googleInitialize msg =
    Effect { action = Google <| Initialize msg }


wiktionary :
    (Result Http.Error Usages -> msg)
    -> String
    -> Effect msg
wiktionary toMsg word =
    Effect { action = Wiktionary <| { word = word, toMsg = toMsg } }


uuid : (String -> msg) -> Effect msg
uuid toMsg =
    Effect { action = Uuid <| SingleUuid toMsg }


uuids : Int -> (List String -> msg) -> Effect msg
uuids num toMsg =
    Effect { action = Uuid <| MultipleUuids num toMsg }


toast : ToastAction -> Effect msg
toast action =
    Effect { action = Toast action }



-- ACCESSORS


match :
    Effect msg
    ->
        { onNone : a
        , onGoogle : GoogleAction msg -> a
        , onWiktionary : WiktionaryAction msg -> a
        , onUuid : UuidAction msg -> a
        , onToast : ToastAction -> a
        }
    -> a
match (Effect effect) { onNone, onGoogle, onWiktionary, onUuid, onToast } =
    case effect.action of
        None ->
            onNone

        Google googleAction ->
            onGoogle googleAction

        Wiktionary request ->
            onWiktionary request

        Uuid toMsg ->
            onUuid toMsg

        Toast config ->
            onToast config



-- TRANSFORMERS


map : (a -> msg) -> Effect a -> Effect msg
map callback (Effect effect) =
    Effect
        { action =
            case effect.action of
                None ->
                    None

                Google (Initialize rootMsg) ->
                    Google <| Initialize <| (callback << rootMsg)

                Google (SendRequest cmd) ->
                    Google <| SendRequest <| SheetsCmd.map callback cmd

                Wiktionary request ->
                    Wiktionary
                        { word = request.word
                        , toMsg = request.toMsg >> callback
                        }

                Uuid (SingleUuid uuidToMsg) ->
                    Uuid <| SingleUuid (uuidToMsg >> callback)

                Uuid (MultipleUuids num uuidToMsg) ->
                    Uuid <| MultipleUuids num (uuidToMsg >> callback)

                Toast config ->
                    Toast config
        }



-- GOOGLE ACTIONS


type GoogleAction msg
    = Initialize (InitializeUpdate -> msg)
    | SendRequest (SheetsCmd msg)


type InitializeUpdate
    = InitializingApi
    | AuthenticatingApi
    | LocatingAppFolder
    | CreatingAppFolder
    | LocatingMainSheet
    | CreatingMainSheet
    | CheckingMainSheetMigrations
    | MigratingMainSheet String
    | Done
    | Failed InitializeFailure


type InitializeFailure
    = ApiAuthentication TaskPort.Error
    | AppFolderLocation Task.Error
    | AppFolderCreation Task.Error
    | MainSheetLocation Task.Error
    | MainSheetCreation Task.Error
    | MainSheetMigration String Task.Error



-- WICTIONARY ACTIONS


type alias WiktionaryAction msg =
    { word : String
    , toMsg : Result Http.Error Usages -> msg
    }



-- TODO: move these if possible


type Usages
    = Usages (List Definitions)


type Definitions
    = Definitions (List Definition)


type alias Definition =
    { text : String
    , examples : List Example
    , formUsages : List FormUsages
    }


type alias FormUsages =
    { word : String
    , usages : Usages
    }


type alias Example =
    { example : String
    , translation : Maybe String
    }



-- UUID ACTIONS


type UuidAction rootMsg
    = SingleUuid (String -> rootMsg)
    | MultipleUuids Int (List String -> rootMsg)



-- TOAST ACTIONS


type alias ToastAction =
    { category : ToastCategory
    , title : String
    , description : String
    }


type ToastCategory
    = ToastSuccess
    | ToastError
    | ToastInfo
    | ToastWarning
