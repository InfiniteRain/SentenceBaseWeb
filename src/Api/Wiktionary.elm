module Api.Wiktionary exposing
    ( Definition
    , Definitions(..)
    , Example
    , FormUsages
    , Model
    , Msg(..)
    , RequestConfig
    , Usages(..)
    , init
    , subscriptions
    , update
    )

import Api.OutMsg as OutMsg exposing (OutMsg(..))
import Dict exposing (Dict)
import Html.Parser as Parser exposing (Node(..))
import Http
import Json.Decode as Decode exposing (Decoder)
import Regex
import RegexExtra
import Set exposing (Set)
import Url.Builder exposing (crossOrigin)



-- MODEL


type alias Model =
    { responseCache : Dict String Response
    }


init : ( Model, Cmd (Msg rootMsg) )
init =
    ( { responseCache = Dict.empty
      }
    , Cmd.none
    )



-- UPDATE


type Msg rootMsg
    = SentRequest (RequestConfig rootMsg)
    | ReceivedResponse String (RequestCtx rootMsg) (Result Http.Error Response)


type alias RequestCtx rootMsg =
    { state : RequestState
    , config : RequestConfig rootMsg
    , results : List (Result Http.Error Usages)
    }


type RequestState
    = RequestBaseWord
    | RequestFormWords
        { usages : Usages
        , formWords : List String
        }


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


update : Msg rootMsg -> Model -> ( Model, Cmd (Msg rootMsg), OutMsg rootMsg )
update msg model =
    case msg of
        SentRequest request ->
            case request.words of
                word :: rest ->
                    getFromCacheOrRequest
                        model
                        word
                        { state = RequestBaseWord
                        , config = { request | words = rest }
                        , results = []
                        }

                [] ->
                    ( model
                    , Cmd.none
                    , OutMsg.some <| request.toMsg []
                    )

        ReceivedResponse word ({ state, config, results } as ctx) result ->
            let
                newModel =
                    result
                        |> Result.map (putToCache word model)
                        |> Result.withDefault model
            in
            case ( state, result ) of
                ( RequestBaseWord, Err err ) ->
                    case config.words of
                        nextWord :: restWords ->
                            getFromCacheOrRequest
                                newModel
                                nextWord
                                { state = RequestBaseWord
                                , config = { config | words = restWords }
                                , results = results ++ [ Err err ]
                                }

                        [] ->
                            ( newModel
                            , Cmd.none
                            , OutMsg.some <|
                                config.toMsg (results ++ [ Err err ])
                            )

                ( RequestBaseWord, Ok response ) ->
                    let
                        usages =
                            responseToUsages word True response

                        formWords =
                            getFormWords usages
                    in
                    case ( config.words, formWords ) of
                        ( _, nextFormWord :: rest ) ->
                            getFromCacheOrRequest
                                newModel
                                nextFormWord
                                { ctx
                                    | state =
                                        RequestFormWords
                                            { usages = usages
                                            , formWords = rest
                                            }
                                }

                        ( nextWord :: restWords, [] ) ->
                            getFromCacheOrRequest
                                newModel
                                nextWord
                                { state = RequestBaseWord
                                , config = { config | words = restWords }
                                , results = results ++ [ Ok usages ]
                                }

                        ( [], [] ) ->
                            ( newModel
                            , Cmd.none
                            , OutMsg.some <|
                                config.toMsg
                                    (results ++ [ Ok usages ])
                            )

                ( RequestFormWords { usages, formWords }, formResult ) ->
                    let
                        newUsages =
                            case formResult of
                                Ok response ->
                                    setFormWordInUsages
                                        word
                                        (responseToUsages
                                            word
                                            False
                                            response
                                        )
                                        usages

                                _ ->
                                    usages
                    in
                    case ( config.words, formWords ) of
                        ( _, nextFormWord :: rest ) ->
                            getFromCacheOrRequest
                                newModel
                                nextFormWord
                                { ctx
                                    | state =
                                        RequestFormWords
                                            { usages = newUsages
                                            , formWords = rest
                                            }
                                }

                        ( nextWord :: restWords, [] ) ->
                            getFromCacheOrRequest
                                newModel
                                nextWord
                                { state = RequestBaseWord
                                , config = { config | words = restWords }
                                , results = results ++ [ Ok newUsages ]
                                }

                        ( [], [] ) ->
                            ( newModel
                            , Cmd.none
                            , OutMsg.some <|
                                config.toMsg
                                    (results ++ [ Ok newUsages ])
                            )


sendRequest : String -> RequestCtx rootMsg -> Cmd (Msg rootMsg)
sendRequest word state =
    Http.get
        { url = wiktionaryUrl word
        , expect = Http.expectJson (ReceivedResponse word state) responseDecoder
        }


putToCache : String -> Model -> Response -> Model
putToCache word model response =
    { model | responseCache = Dict.insert word response model.responseCache }


getFromCacheOrRequest :
    Model
    -> String
    -> RequestCtx rootMsg
    -> ( Model, Cmd (Msg rootMsg), OutMsg rootMsg )
getFromCacheOrRequest model word state =
    case Dict.get word model.responseCache of
        Just response ->
            update (ReceivedResponse word state (Ok response)) model

        Nothing ->
            ( model, sendRequest word state, OutMsg.none )


getFormWords : Usages -> List String
getFormWords (Usages usages) =
    usages
        |> List.concatMap (\(Definitions definitions) -> definitions)
        |> List.concatMap .formUsages
        |> List.map .word
        |> Set.fromList
        |> Set.toList


setFormWordInUsages : String -> Usages -> Usages -> Usages
setFormWordInUsages word usagesOfFormWord (Usages usages) =
    List.map
        (\(Definitions definitions) ->
            List.map
                (\definition ->
                    { definition
                        | formUsages =
                            List.map
                                (\formUsage ->
                                    { formUsage
                                        | usages =
                                            if formUsage.word == word then
                                                usagesOfFormWord

                                            else
                                                formUsage.usages
                                    }
                                )
                                definition.formUsages
                    }
                )
                definitions
                |> Definitions
        )
        usages
        |> Usages



-- SUBSCRIPTIONS


subscriptions : Model -> Sub (Msg rootMsg)
subscriptions _ =
    Sub.none



-- EXTERNAL API


type alias RequestConfig msg =
    { words : List String
    , toMsg : List (Result Http.Error Usages) -> msg
    }



-- DECODERS


type alias Response =
    { nl : Maybe (List UsageResponse) }


responseDecoder : Decode.Decoder Response
responseDecoder =
    Decode.map Response
        (Decode.maybe (Decode.field "nl" (Decode.list usageDecoder)))


type alias UsageResponse =
    { partOfSpeech : String
    , definitions : List DefinitionResponse
    }


usageDecoder : Decoder UsageResponse
usageDecoder =
    Decode.map2 UsageResponse
        (Decode.field "partOfSpeech" Decode.string)
        (Decode.field "definitions" (Decode.list definitionDecoder))


type alias DefinitionResponse =
    { description : Maybe String
    , definition : String
    , parsedExamples : Maybe (List ParsedExampleResponse)
    }


definitionDecoder : Decoder DefinitionResponse
definitionDecoder =
    Decode.map3 DefinitionResponse
        (Decode.maybe (Decode.field "description" Decode.string))
        (Decode.field "definition" Decode.string)
        (Decode.maybe
            (Decode.field "parsedExamples"
                (Decode.list parsedExamplesDecoder)
            )
        )


type alias ParsedExampleResponse =
    { example : String
    , translation : Maybe String
    }


parsedExamplesDecoder : Decoder ParsedExampleResponse
parsedExamplesDecoder =
    Decode.map2 ParsedExampleResponse
        (Decode.field "example" Decode.string)
        (Decode.maybe (Decode.field "translation" Decode.string))



-- HTML PARSING


collectBaseFormFordsFromHtml : String -> String -> List String
collectBaseFormFordsFromHtml currentWord response =
    response
        |> Parser.run
        |> Result.toMaybe
        |> Maybe.withDefault []
        |> collectBaseFormWordsFromNode Set.empty
        |> Set.toList
        |> List.filter ((/=) currentWord)


collectBaseFormWordsFromNode : Set String -> List Parser.Node -> Set String
collectBaseFormWordsFromNode words nodes =
    case nodes of
        (Parser.Text textStr) :: ((Parser.Element "span" attrs _) as element) :: rest ->
            let
                aElement =
                    findElement "a" element

                formWord =
                    aElement
                        |> Maybe.map nodeToText
                        |> Maybe.withDefault ""

                endsWithOf =
                    String.trim textStr
                        |> String.endsWith "of"

                hasSpecialClass =
                    hasClass "form-of-definition-link" attrs

                hasDutchLinkHref =
                    aElement
                        |> Maybe.andThen (getAttrStrFromNode "href")
                        |> Maybe.map (String.endsWith "#Dutch")
                        |> Maybe.withDefault False
            in
            collectBaseFormWordsFromNode
                (if endsWithOf && hasSpecialClass && hasDutchLinkHref then
                    Set.insert formWord words

                 else
                    words
                )
                (element :: rest)

        (Parser.Element _ _ childNodes) :: rest ->
            collectBaseFormWordsFromNode
                (Set.union words
                    (collectBaseFormWordsFromNode words childNodes)
                )
                rest

        _ :: rest ->
            collectBaseFormWordsFromNode words rest

        [] ->
            words


hasClass : String -> List Parser.Attribute -> Bool
hasClass className attrs =
    getAttrStr "class" attrs
        |> Maybe.map extractClasses
        |> Maybe.map (List.any ((==) className))
        |> Maybe.withDefault False


extractClasses : String -> List String
extractClasses classesStr =
    classesStr
        |> String.trim
        |> Regex.split RegexExtra.space


getAttrStrFromNode : String -> Node -> Maybe String
getAttrStrFromNode attr node =
    case node of
        Element _ attrs _ ->
            getAttrStr attr attrs

        _ ->
            Nothing


getAttrStr : String -> List Parser.Attribute -> Maybe String
getAttrStr attr attrs =
    case attrs of
        ( attrName, classesStr ) :: rest ->
            if attr == attrName then
                Just classesStr

            else
                getAttrStr attr rest

        [] ->
            Nothing


findElement : String -> Node -> Maybe Node
findElement elementName node =
    case node of
        Element name attrs (child :: rest) ->
            if elementName == name then
                Just node

            else
                case findElement elementName child of
                    Just foundNode ->
                        Just foundNode

                    Nothing ->
                        findElement elementName (Element name attrs rest)

        _ ->
            Nothing


htmlToText : String -> String
htmlToText html =
    html
        |> Regex.replace RegexExtra.styleTag (\_ -> "")
        |> Regex.replace RegexExtra.tag (\_ -> "")
        |> String.trim


nodeToText : Parser.Node -> String
nodeToText node =
    htmlToText (Parser.nodeToString node)



-- TRANSFORMERS


responseToUsages : String -> Bool -> Response -> Usages
responseToUsages currentWord doFormLookup response =
    response.nl
        |> Maybe.map
            (List.map
                (usageResponseToDefinitions currentWord doFormLookup)
            )
        |> Maybe.withDefault []
        |> Usages


usageResponseToDefinitions : String -> Bool -> UsageResponse -> Definitions
usageResponseToDefinitions currentWord doFormLookup usage =
    List.map
        (definitionResponseToDefinition currentWord doFormLookup)
        usage.definitions
        |> List.filter (\definition -> definition.text /= "")
        |> Definitions


definitionResponseToDefinition :
    String
    -> Bool
    -> DefinitionResponse
    -> Definition
definitionResponseToDefinition currentWord doFormLookup definition =
    { text = htmlToText definition.definition
    , examples =
        Maybe.withDefault [] definition.parsedExamples
            |> List.map parsedExampleResponseToExample
    , formUsages =
        if doFormLookup then
            collectBaseFormFordsFromHtml currentWord definition.definition
                |> List.map
                    (\word ->
                        { word = word
                        , usages = Usages []
                        }
                    )

        else
            []
    }


parsedExampleResponseToExample : ParsedExampleResponse -> Example
parsedExampleResponseToExample parsedExample =
    { example = htmlToText parsedExample.example
    , translation = Maybe.map htmlToText parsedExample.translation
    }



-- BUILDERS


wiktionaryUrl : String -> String
wiktionaryUrl word =
    crossOrigin
        "https://en.wiktionary.org/api/rest_v1/page/definition"
        [ word ]
        []
