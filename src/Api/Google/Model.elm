module Api.Google.Model exposing
    ( MinedSentence
    , PendingSentence
    , fromGridData
    , fromGridDataMatrix
    )

import Api.Google.Exchange.Sheets as Sheets
import Time



-- TYPES


type alias PendingSentence =
    { word : String
    , sentence : String
    , tags : List String
    , addedAt : Time.Posix
    }


type alias MinedSentence =
    { word : String
    , sentence : String
    , tags : List String
    , batchId : String
    , minedAt : Time.Posix
    }



-- CONSTRUCTORS


fromGridData :
    (List Sheets.ResponseCellData -> Maybe a)
    -> Sheets.ResponseGetSubSheetData
    -> List a
fromGridData maybeConstructor =
    .sheets
        >> List.head
        >> Maybe.map .data
        >> Maybe.andThen List.head
        >> Maybe.andThen .rowData
        >> Maybe.withDefault []
        >> List.filterMap
            (.values
                >> Maybe.withDefault []
                >> maybeConstructor
            )


fromGridDataMatrix :
    (List Sheets.ResponseCellData -> ( Maybe a, List Sheets.ResponseCellData ))
    -> Sheets.ResponseGetSubSheetData
    -> List (List a)
fromGridDataMatrix maybeConstructor =
    .sheets
        >> List.head
        >> Maybe.map .data
        >> Maybe.andThen List.head
        >> Maybe.andThen .rowData
        >> Maybe.withDefault []
        >> List.map
            (.values
                >> Maybe.withDefault []
                >> maybeConstructor
            )
        >> List.unzip
        >> Tuple.mapFirst (\a -> [ a ])
        >> formGridDataMatrixAux maybeConstructor


formGridDataMatrixAux :
    (List Sheets.ResponseCellData -> ( Maybe a, List Sheets.ResponseCellData ))
    -> ( List (List (Maybe a)), List (List Sheets.ResponseCellData) )
    -> List (List a)
formGridDataMatrixAux maybeConstructor ( parsed, rows ) =
    case
        List.map maybeConstructor rows
            |> List.unzip
    of
        ( _, [] ) ->
            []

        ( newParsed, [] :: _ ) ->
            (newParsed :: parsed)
                |> List.map (List.filterMap identity)
                |> List.filter (not << List.isEmpty)
                |> List.reverse

        ( newParsed, restRows ) ->
            formGridDataMatrixAux maybeConstructor
                ( newParsed :: parsed, restRows )
