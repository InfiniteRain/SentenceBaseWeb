module Api.Google.Model exposing
    ( MinedSentence
    , PendingSentence
    , fromGridData
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
        >> List.map
            (.values
                >> Maybe.withDefault []
                >> maybeConstructor
            )
        >> List.filterMap identity
