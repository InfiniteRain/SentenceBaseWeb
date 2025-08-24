module Page.Batches exposing (Model, Msg(..), init, subscriptions, update, view)

import Api.Google.Constants as Constants exposing (SubSheet(..))
import Api.Google.Exchange.Sheets as Sheets
    exposing
        ( RequestBatchUpdateKind(..)
        , RequestDimension(..)
        , RequestExtendedValue(..)
        , requestRow
        )
import Api.Google.Exchange.Task as Task
import Api.Google.ListConstructor
    exposing
        ( cellNumberValue
        , cellStringValue
        , constructFromList
        , extractWithRest
        , field
        )
import Api.Google.Model as Model
import Basecoat exposing (classes)
import Effect exposing (Effect)
import Html exposing (Html, button, div, h2, label, p, section, text)
import Html.Attributes exposing (class, disabled, type_)
import Html.Events exposing (onClick)
import Icon.Info exposing (infoIcon)
import Icon.WarningCircle exposing (warningCircleIcon)
import Json.Decode as Decode
import Port.Anki as Anki exposing (ModelRequiredKind(..))
import Session exposing (Session)
import Task as PlatformTask
import Time exposing (Month(..))



-- MODEL


type alias Model =
    { session : Session
    , currentPage : Int
    , currentBatches : List (List Model.MinedSentence)
    , reachedEnd : Bool
    , getState : GetState
    }


type GetState
    = GetLoading
    | GetSuccess
    | GetFail Task.Error


init : Session -> ( Model, Cmd Msg, Effect Msg )
init session =
    ( { session = session
      , currentPage = 0
      , currentBatches = []
      , reachedEnd = False
      , getState = GetLoading
      }
    , Cmd.none
    , getBatchesEffect 0
    )



-- UPDATE


type Msg
    = GetFetched (Result Task.Error (List (List Model.MinedSentence)))
    | LoadMoreClicked
    | BatchClicked (List Model.MinedSentence)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg, Effect Msg )
update msg model =
    case msg of
        GetFetched (Ok batches) ->
            ( { model
                | getState = GetSuccess
                , currentPage = model.currentPage + 1
                , currentBatches = model.currentBatches ++ batches
                , reachedEnd = List.length batches < pageSize
              }
            , Cmd.none
            , Effect.none
            )

        GetFetched (Err err) ->
            ( { model | getState = GetFail err }
            , Cmd.none
            , Effect.none
            )

        LoadMoreClicked ->
            ( { model | getState = GetLoading }
            , Cmd.none
            , getBatchesEffect model.currentPage
            )

        BatchClicked batch ->
            ( model
            , Anki.export
                "export.apkg"
                (Anki.addNotes
                    (List.map
                        (\sentence ->
                            [ sentence.sentence
                            , ""
                            , sentence.word
                            , ""
                            , ""
                            ]
                        )
                        batch
                    )
                    ankiModel
                    ankiDeck
                )
                |> PlatformTask.attempt (\_ -> NoOp)
            , Effect.none
            )

        NoOp ->
            ( model, Cmd.none, Effect.none )


getBatchesRequest : Int -> Task.SheetsTask (List (List Model.MinedSentence))
getBatchesRequest page =
    Sheets.batchUpdateAndGetGridDataRequest
        [ RequestDeleteRange
            { range =
                { sheetId = Constants.subSheetId Query
                , startRowIndex = Just 0
                , endRowIndex = Nothing
                , startColumnIndex = Just 0
                , endColumnIndex = Nothing
                }
            , dimension = RequestRows
            }
        , RequestUpdateCells
            { rows =
                requestRow
                    [ RequestFormula <|
                        "=QUERY("
                            ++ Constants.subSheetName MinedSentences
                            ++ "!A2:E,\""
                            ++ "SELECT COUNT(D), D, AVG(E) "
                            ++ "WHERE D <> '' "
                            ++ "GROUP BY D "
                            ++ "ORDER BY AVG(E) DESC "
                            ++ "LIMIT "
                            ++ String.fromInt pageSize
                            ++ " OFFSET "
                            ++ (String.fromInt <| page * pageSize)
                            ++ "\")"
                    ]
            , fields = "userEnteredValue"
            , range =
                { sheetId = Constants.subSheetId Query
                , startRowIndex = Just 0
                , endRowIndex = Just 1
                , startColumnIndex = Just 0
                , endColumnIndex = Just 1
                }
            }
        ]
        [ Constants.subSheetName Query ++ "!B2:B" ]
        |> Task.map
            (.updatedSpreadsheet >> Model.fromGridData constructBatchIds)
        |> Task.andThen
            (\ids ->
                Sheets.batchUpdateAndGetGridDataRequest
                    (RequestDeleteRange
                        { range =
                            { sheetId = Constants.subSheetId Query
                            , startRowIndex = Just 0
                            , endRowIndex = Nothing
                            , startColumnIndex = Just 0
                            , endColumnIndex = Nothing
                            }
                        , dimension = RequestRows
                        }
                        :: List.indexedMap
                            (\index id ->
                                RequestUpdateCells
                                    { rows =
                                        requestRow
                                            [ RequestFormula <|
                                                "=QUERY("
                                                    ++ Constants.subSheetName
                                                        MinedSentences
                                                    ++ "!A1:E,\""
                                                    ++ "SELECT * WHERE D = '"
                                                    ++ id
                                                    ++ "' OR D = 'batch_id'"
                                                    ++ "\")"
                                            ]
                                    , fields = "userEnteredValue"
                                    , range =
                                        { sheetId = Constants.subSheetId Query
                                        , startRowIndex = Just 0
                                        , endRowIndex = Nothing
                                        , startColumnIndex = Just (index * 5)
                                        , endColumnIndex =
                                            Just
                                                ((index * 5) + 5)
                                        }
                                    }
                            )
                            ids
                    )
                    [ Constants.subSheetName Query ++ "!A2:ZZ" ]
            )
        |> Task.map
            (.updatedSpreadsheet
                >> Model.fromGridDataMatrix maybeConstructMinedSentence
            )


constructBatchIds : List Sheets.ResponseCellData -> Maybe String
constructBatchIds =
    List.head >> Maybe.andThen cellStringValue


maybeConstructMinedSentence :
    List Sheets.ResponseCellData
    -> ( Maybe Model.MinedSentence, List Sheets.ResponseCellData )
maybeConstructMinedSentence =
    constructFromList Model.MinedSentence
        >> field cellStringValue
        >> field cellStringValue
        >> field
            (cellStringValue
                >> Maybe.map (Decode.decodeString (Decode.list Decode.string))
                >> Maybe.andThen Result.toMaybe
            )
        >> field cellStringValue
        >> field
            (cellNumberValue
                >> Maybe.map round
                >> Maybe.map Time.millisToPosix
            )
        >> extractWithRest


getBatchesEffect : Int -> Effect Msg
getBatchesEffect page =
    getBatchesRequest page
        |> Task.sheetsAttempt GetFetched
        |> Effect.google



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Batches"
    , content =
        div
            [ classes
                [ "flex"
                , "shrink-0"
                , "grow-0"
                , "justify-center"
                , "h-full"
                ]
            ]
            [ batchesView model ]
    }


batchesView : Model -> Html Msg
batchesView model =
    div [ classes [ "flex", "flex-col", "basis-md", "h-full" ] ]
        [ div
            [ classes
                [ "grow-1"
                , "shrink-0"
                , "overflow-auto"
                , "basis-0"
                ]
            ]
          <|
            List.concat
                [ List.map (batchView model) model.currentBatches
                , case ( model.getState, model.currentBatches ) of
                    ( GetLoading, _ ) ->
                        [ batchSkeletonView [ "w-[95%]", "w-[30%]" ]
                        , batchSkeletonView [ "w-[85%]" ]
                        , batchSkeletonView [ "w-[97%]", "w-[50%]" ]
                        ]

                    ( GetSuccess, [] ) ->
                        [ div [ class "alert" ]
                            [ infoIcon []
                            , h2 [] [ text "No batches have yet been made." ]
                            ]
                        ]

                    ( GetSuccess, _ ) ->
                        []

                    ( GetFail err, _ ) ->
                        [ div
                            [ classes
                                [ "alert-destructive"
                                , "[&:not(:first-child)]:mt-4"
                                ]
                            ]
                            [ warningCircleIcon []
                            , h2 [] [ text "Unable to fetch batches." ]
                            , section [] [ text <| Task.errorToMessage err ]
                            ]
                        ]
                , if
                    model.currentPage
                        == 1
                        && List.isEmpty model.currentBatches
                  then
                    []

                  else
                    [ button
                        [ classes [ "btn-ghost", "w-full", "mt-4" ]
                        , disabled
                            (model.getState
                                == GetLoading
                                || model.reachedEnd
                            )
                        , onClick LoadMoreClicked
                        ]
                        [ text <|
                            if model.reachedEnd then
                                "No more batches to load"

                            else
                                "Load more"
                        ]
                    ]
                ]
        ]


batchSkeletonView : List String -> Html Msg
batchSkeletonView wordsSizes =
    div
        [ classes
            [ "flex"
            , "flex-col"
            , "items-start"
            , "gap-3"
            , "border"
            , "p-3"
            , "[&:not(:first-child)]:mt-4"
            ]
        ]
    <|
        List.map
            (\wordsSize ->
                div
                    [ classes
                        [ "bg-accent"
                        , "animate-pulse"
                        , "rounded-md"
                        , "h-4"
                        , wordsSize
                        ]
                    ]
                    []
            )
            wordsSizes


batchView : Model -> List Model.MinedSentence -> Html Msg
batchView model batch =
    let
        batchedAt =
            List.head batch
                |> Maybe.map .minedAt
                |> Maybe.withDefault (Time.millisToPosix 0)
    in
    label
        [ classes
            [ "flex"
            , "items-start"
            , "gap-3"
            , "border"
            , "p-3"
            , "hover:bg-accent/50"
            , "rounded-lg"
            , "has-[button:disabled]:opacity-50"
            , "[&:not(:first-child)]:mt-4"
            ]
        ]
        [ button
            [ type_ "button"
            , onClick (BatchClicked batch)
            ]
            []
        , div [ classes [ "grid", "gap-2" ] ] <|
            [ h2
                [ classes
                    [ "text-sm"
                    , "leading-none"
                    , "font-medium"
                    ]
                ]
                [ text
                    ("Mined on "
                        ++ posixToString
                            (Session.zone model.session)
                            batchedAt
                    )
                ]
            , p
                [ class "text-sm" ]
                [ text
                    (batch
                        |> List.map .word
                        |> String.join ", "
                    )
                ]
            ]
        ]


posixToString : Time.Zone -> Time.Posix -> String
posixToString zone posix =
    (Time.toDay zone posix |> String.fromInt)
        ++ " "
        ++ (Time.toMonth zone posix |> monthToString)
        ++ " "
        ++ (Time.toYear zone posix |> String.fromInt)
        ++ ", "
        ++ (Time.toHour zone posix |> String.fromInt |> String.padLeft 2 '0')
        ++ ":"
        ++ (Time.toMinute zone posix |> String.fromInt |> String.padLeft 2 '0')


monthToString : Time.Month -> String
monthToString month =
    case month of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"



-- CONSTANTS


pageSize : Int
pageSize =
    10


ankiModel : Anki.Model
ankiModel =
    { id = 1755984462852
    , name = "Sentence Base Model"
    , fields =
        [ { name = "Sentence" }
        , { name = "Sentence Translation" }
        , { name = "Word" }
        , { name = "Word Definition" }
        , { name = "Sentence Audio" }
        ]
    , templates =
        [ { name = Nothing
          , frontHtml = String.trim """
<div class="body front-notice">Audio Only</div>

<div class="footer">{{Sentence Audio}}</div>
"""
          , backHtml = String.trim """
<div class="body">
  <div class="sentence">
    <div class="bigger">{{Sentence}}</div>
    <div class="bigger">{{Sentence Translation}}</div>
  </div>
  <hr />
  <div class="bigger">{{Word}}</div>
  <div class="definition">{{Word Definition}}</div>
</div>

<div class="footer">{{Sentence Audio}}</div>
"""
          }
        ]
    , styling = String.trim """
html,
body,
body>div {
  padding: 0;
  margin: 0;
  height: 100%;
}

.body {
  overflow: auto;
  text-align: center;
  height: calc(100% - 64px);
}

.footer {
  position: absolute;
  bottom: 0;
  height: 48px;
  width: 100%;
  display: flex;
  align-items: center;
  justify-content: center;
}

.front-notice {
  display: flex;
  align-items: center;
  justify-content: center;
  font-style: italic;
  color: #7e7f7a;
}

.bigger {
  font-size: 32px;
}

.sentence div {
  margin: 16px;
}

.definition {
  text-align: start;
  padding: 16px 48px 0 64px;
}

.definition ul {
  list-style: square;
  margin: 8px 0 0 0;
}

.definition ul:last-child {
  margin-bottom: 8px;
}

.definition li {
  margin-bottom: 4px;
}

.definition .etimology {
  font-size: 18px;
  margin-top: 8px;
}

.definition {
  font-size: 16px;
}
"""
    }


ankiDeck : Anki.Deck
ankiDeck =
    Anki.deck 1755984963683 "Sentence Base"
