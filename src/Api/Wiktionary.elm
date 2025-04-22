module Api.Wiktionary exposing (..)

import Http
import Json.Decode as Decode exposing (Decoder)
import Url exposing (Protocol(..))
import Url.Builder exposing (crossOrigin)



-- API


wiktionaryRequest : String -> (Result Http.Error WiktionaryResponse -> msg) -> Cmd msg
wiktionaryRequest word msg =
    Http.get
        { url = wiktionaryUrl word
        , expect = Http.expectJson msg wiktionaryResponseDecoder
        }



-- DECODERS


type alias WiktionaryResponse =
    { nl : Maybe (List WiktionaryUsage) }


wiktionaryResponseDecoder : Decode.Decoder WiktionaryResponse
wiktionaryResponseDecoder =
    Decode.map WiktionaryResponse
        (Decode.maybe (Decode.field "nl" (Decode.list wiktionaryUsageDecoder)))


type alias WiktionaryUsage =
    { partOfSpeech : String
    , definitions : List WiktionaryDefinition
    }


wiktionaryUsageDecoder : Decoder WiktionaryUsage
wiktionaryUsageDecoder =
    Decode.map2 WiktionaryUsage
        (Decode.field "partOfSpeech" Decode.string)
        (Decode.field "definitions" (Decode.list wiktionaryDefinitionDecoder))


type alias WiktionaryDefinition =
    { description : Maybe String
    , definition : String
    , examples : Maybe (List String)
    }


wiktionaryDefinitionDecoder : Decoder WiktionaryDefinition
wiktionaryDefinitionDecoder =
    Decode.map3 WiktionaryDefinition
        (Decode.maybe (Decode.field "description" Decode.string))
        (Decode.field "definition" Decode.string)
        (Decode.maybe (Decode.field "examples" (Decode.list Decode.string)))


type alias WiktionaryParsedExample =
    { example : String
    , translation : Maybe String
    }


wiktionaryParsedExampleDecoder : Decoder WiktionaryParsedExample
wiktionaryParsedExampleDecoder =
    Decode.map2 WiktionaryParsedExample
        (Decode.field "example" Decode.string)
        (Decode.maybe (Decode.field "translation" Decode.string))



-- BUILDERS


wiktionaryUrl : String -> String
wiktionaryUrl word =
    crossOrigin "https://en.wiktionary.org/api/rest_v1/page/definition" [ word ] []
