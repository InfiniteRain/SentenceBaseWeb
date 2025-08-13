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

import Dict exposing (Dict)
import Html.Parser as Parser exposing (Node(..))
import Http
import Json.Decode as Decode exposing (Decoder)
import OutMsg exposing (OutMsg(..))
import Regex
import RegexExtra
import Set exposing (Set)
import Triple
import Url.Builder exposing (crossOrigin)



-- MODEL


type alias Model rootMsg =
    { state : State
    , requestQueue : List (RequestConfig rootMsg)
    , responseCache : Dict String Response
    }


type State
    = Ready
    | ResolvingForm Usages (List String)


type Usages
    = Usages (List Definitions)


responseToUsages : String -> Bool -> Response -> Usages
responseToUsages currentWord doFormLookup response =
    response.nl
        |> Maybe.map (List.map (usageResponseToDefinitions currentWord doFormLookup))
        |> Maybe.withDefault []
        |> Usages


type Definitions
    = Definitions (List Definition)


usageResponseToDefinitions : String -> Bool -> UsageResponse -> Definitions
usageResponseToDefinitions currentWord doFormLookup usage =
    List.map (definitionResponseToDefinition currentWord doFormLookup) usage.definitions
        |> List.filter (\definition -> definition.text /= "")
        |> Definitions


type alias Definition =
    { text : String
    , examples : List Example
    , formUsages : List FormUsages
    }


type alias FormUsages =
    { word : String
    , usages : Usages
    }


definitionResponseToDefinition : String -> Bool -> DefinitionResponse -> Definition
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


type alias Example =
    { example : String
    , translation : Maybe String
    }


parsedExampleResponseToExample : ParsedExampleResponse -> Example
parsedExampleResponseToExample parsedExample =
    { example = htmlToText parsedExample.example
    , translation = Maybe.map htmlToText parsedExample.translation
    }


init : () -> ( Model rootMsg, Cmd (Msg rootMsg) )
init _ =
    ( { state = Ready
      , requestQueue = []
      , responseCache = Dict.empty
      }
    , Cmd.none
    )



-- UPDATE


type Msg rootMsg
    = SentRequest (RequestConfig rootMsg)
    | ReceivedResponse (Result Http.Error Response)


update : Msg rootMsg -> Model rootMsg -> ( Model rootMsg, Cmd (Msg rootMsg), OutMsg rootMsg )
update msg model =
    case ( model.state, msg, model.requestQueue ) of
        ( Ready, SentRequest request, [] ) ->
            getFromCacheOrUpdate
                request.word
                ( { model | requestQueue = model.requestQueue ++ [ request ] }
                , sendRequest request
                , OutMsg.none
                )

        ( Ready, SentRequest request, _ ) ->
            ( { model | requestQueue = model.requestQueue ++ [ request ] }
            , Cmd.none
            , OutMsg.none
            )

        ( Ready, ReceivedResponse ((Ok response) as result), request :: restRequests ) ->
            let
                usages =
                    responseToUsages request.word True response

                formWords =
                    getFormWords usages
            in
            case ( formWords, restRequests ) of
                ( formWord :: _, _ ) ->
                    getFromCacheOrUpdate
                        formWord
                        ( putToCache
                            request.word
                            result
                            { model | state = ResolvingForm usages (getFormWords usages) }
                        , getDefinitionsInternal formWord
                        , OutMsg.none
                        )

                ( [], nextRequest :: _ ) ->
                    getFromCacheOrUpdate
                        nextRequest.word
                        ( putToCache
                            request.word
                            result
                            { model | requestQueue = restRequests }
                        , sendRequest nextRequest
                        , OutMsg.some <| request.toMsg (Ok usages)
                        )

                ( [], [] ) ->
                    ( putToCache
                        request.word
                        result
                        { model | requestQueue = [] }
                    , Cmd.none
                    , OutMsg.some <| request.toMsg (Ok usages)
                    )

        ( Ready, ReceivedResponse (Err err), request :: restRequests ) ->
            case restRequests of
                nextRequest :: _ ->
                    getFromCacheOrUpdate
                        nextRequest.word
                        ( { model | requestQueue = restRequests }
                        , sendRequest nextRequest
                        , OutMsg.some <| request.toMsg (Err err)
                        )

                [] ->
                    ( { model | requestQueue = [] }
                    , Cmd.none
                    , OutMsg.some <| request.toMsg (Err err)
                    )

        ( ResolvingForm _ _, SentRequest request, _ ) ->
            ( { model | requestQueue = model.requestQueue ++ [ request ] }
            , Cmd.none
            , OutMsg.none
            )

        ( ResolvingForm usages (formWord :: restFormWords), ReceivedResponse ((Ok response) as result), request :: restRequests ) ->
            let
                formWordUsages =
                    responseToUsages formWord False response

                newUsages =
                    setFormWordInUsages formWord formWordUsages usages
            in
            case ( restFormWords, restRequests ) of
                ( nextFormWord :: _, _ ) ->
                    getFromCacheOrUpdate
                        nextFormWord
                        ( putToCache
                            formWord
                            result
                            { model | state = ResolvingForm newUsages restFormWords }
                        , getDefinitionsInternal nextFormWord
                        , OutMsg.none
                        )

                ( [], nextRequest :: _ ) ->
                    getFromCacheOrUpdate
                        nextRequest.word
                        ( putToCache
                            formWord
                            result
                            { model | state = Ready, requestQueue = restRequests }
                        , sendRequest nextRequest
                        , OutMsg.some <| request.toMsg (Ok newUsages)
                        )

                ( [], [] ) ->
                    ( putToCache
                        formWord
                        result
                        { model | state = Ready, requestQueue = [] }
                    , Cmd.none
                    , OutMsg.some <| request.toMsg (Ok newUsages)
                    )

        ( ResolvingForm usages (_ :: restFormWords), ReceivedResponse (Err err), request :: restRequests ) ->
            case ( restFormWords, restRequests ) of
                ( nextFormWord :: _, _ ) ->
                    getFromCacheOrUpdate
                        nextFormWord
                        ( { model | state = ResolvingForm usages restFormWords }
                        , getDefinitionsInternal nextFormWord
                        , OutMsg.none
                        )

                ( [], nextRequest :: _ ) ->
                    getFromCacheOrUpdate
                        nextRequest.word
                        ( { model | state = Ready, requestQueue = restRequests }
                        , sendRequest nextRequest
                        , OutMsg.some <| request.toMsg (Err err)
                        )

                ( [], [] ) ->
                    ( { model | state = Ready, requestQueue = [] }
                    , Cmd.none
                    , OutMsg.some <| request.toMsg (Err err)
                    )

        ( ResolvingForm _ [], ReceivedResponse _, _ ) ->
            ( { model | state = Ready }, Cmd.none, OutMsg.none )

        ( _, ReceivedResponse _, [] ) ->
            ( { model | state = Ready }, Cmd.none, OutMsg.none )


sendRequest : RequestConfig rootMsg -> Cmd (Msg rootMsg)
sendRequest request =
    Http.get
        { url = wiktionaryUrl request.word
        , expect = Http.expectJson ReceivedResponse responseDecoder
        }


putToCache : String -> Result Http.Error Response -> Model rootMsg -> Model rootMsg
putToCache word result model =
    result
        |> Result.toMaybe
        |> Maybe.map
            (\response ->
                { model | responseCache = Dict.insert word response model.responseCache }
            )
        |> Maybe.withDefault model


getFromCacheOrUpdate :
    String
    -> ( Model rootMsg, Cmd (Msg rootMsg), OutMsg rootMsg )
    -> ( Model rootMsg, Cmd (Msg rootMsg), OutMsg rootMsg )
getFromCacheOrUpdate word (( model, _, outMsg ) as orUpdate) =
    case Dict.get word model.responseCache of
        Just response ->
            update (ReceivedResponse (Ok response)) model
                |> Triple.mapThird (OutMsg.combine outMsg)

        Nothing ->
            orUpdate


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


subscriptions : Model rootMsg -> Sub (Msg rootMsg)
subscriptions _ =
    Sub.none



-- EXTERNAL API


type alias RequestConfig msg =
    { word : String
    , toMsg : Result Http.Error Usages -> msg
    }



-- INTERNAL API


getDefinitionsInternal : String -> Cmd (Msg rootMsg)
getDefinitionsInternal word =
    Http.get
        { url = wiktionaryUrl word
        , expect =
            Http.expectJson
                ReceivedResponse
                responseDecoder
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
        (Decode.maybe (Decode.field "parsedExamples" (Decode.list parsedExamplesDecoder)))


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
                (Set.union words (collectBaseFormWordsFromNode words childNodes))
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



-- BUILDERS


wiktionaryUrl : String -> String
wiktionaryUrl word =
    crossOrigin "https://en.wiktionary.org/api/rest_v1/page/definition" [ word ] []
