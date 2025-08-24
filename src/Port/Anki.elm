module Port.Anki exposing
    ( Deck
    , Model
    , ModelField
    , ModelRequiredKind(..)
    , ModelTemplate
    , addFiles
    , addNotes
    , deck
    , export
    )

import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import TaskPort



-- TYPES


type alias Model =
    { id : Int
    , name : String
    , fields : List ModelField
    , templates : List ModelTemplate
    , styling : String
    }


type alias ModelField =
    { name : String }


type alias ModelTemplate =
    { name : Maybe String
    , frontHtml : String
    , backHtml : String
    }


type ModelRequiredKind
    = All
    | Any


type Deck
    = Deck
        { id : Int
        , name : String
        , models : Dict Int Model
        , notes : Dict Int (List (List String))
        , files : List ( String, String )
        }



-- CONSTRUCTORS


deck : Int -> String -> Deck
deck id name =
    Deck
        { id = id
        , name = name
        , models = Dict.empty
        , notes = Dict.empty
        , files = []
        }



-- TRANSFORMERS


addNotes : List (List String) -> Model -> Deck -> Deck
addNotes notes model (Deck targetDeck) =
    let
        existingNotes =
            Dict.get model.id targetDeck.notes
                |> Maybe.withDefault []
    in
    Deck
        { targetDeck
            | models =
                Dict.insert
                    model.id
                    model
                    targetDeck.models
            , notes =
                Dict.insert
                    model.id
                    (existingNotes ++ notes)
                    targetDeck.notes
        }


addFiles : List ( String, String ) -> Deck -> Deck
addFiles files (Deck targetDeck) =
    Deck { targetDeck | files = targetDeck.files ++ files }



-- HELPERS


deckEncoder : Deck -> Encode.Value
deckEncoder (Deck { id, name, models, notes, files }) =
    Encode.object
        [ ( "id", Encode.int id )
        , ( "name", Encode.string name )
        , ( "models"
          , Encode.object <|
                List.map
                    (\( modelId, model ) ->
                        ( String.fromInt modelId
                        , modelEncoder model
                        )
                    )
                    (Dict.toList models)
          )
        , ( "notes"
          , Encode.object <|
                List.map
                    (\( modelId, modelNotes ) ->
                        ( String.fromInt modelId
                        , Encode.list (Encode.list Encode.string) modelNotes
                        )
                    )
                    (Dict.toList notes)
          )
        , ( "files"
          , Encode.list
                (\( fileName, data ) ->
                    Encode.list identity
                        [ Encode.string fileName
                        , Encode.string data
                        ]
                )
                files
          )
        ]


modelEncoder : Model -> Encode.Value
modelEncoder { id, name, fields, templates, styling } =
    Encode.object
        [ ( "id", Encode.int id )
        , ( "name", Encode.string name )
        , ( "fields", Encode.list modelFieldEncoder fields )
        , ( "templates", Encode.list modelTemplateEncoder templates )
        , ( "styling", Encode.string styling )
        ]


modelFieldEncoder : ModelField -> Encode.Value
modelFieldEncoder { name } =
    Encode.object
        [ ( "name", Encode.string name )
        ]


modelTemplateEncoder : ModelTemplate -> Encode.Value
modelTemplateEncoder { name, frontHtml, backHtml } =
    Encode.object
        [ ( "name"
          , case name of
                Just modelName ->
                    Encode.string modelName

                Nothing ->
                    Encode.null
          )
        , ( "frontHtml", Encode.string frontHtml )
        , ( "backHtml", Encode.string backHtml )
        ]



-- PORT CALLS


export : String -> Deck -> TaskPort.Task ()
export fileName =
    TaskPort.call
        { function = "ankiExport"
        , valueDecoder = Decode.succeed ()
        , argsEncoder =
            \targetDeck ->
                Encode.object
                    [ ( "deck", deckEncoder targetDeck )
                    , ( "fileName", Encode.string fileName )
                    ]
        }
