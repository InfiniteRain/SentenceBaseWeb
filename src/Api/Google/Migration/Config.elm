module Api.Google.Migration.Config exposing (Config)

import Api.Google.Exchange.SheetsCmd exposing (SheetsCmd)



-- TYPES


type alias Config model msg =
    { id : String
    , model : model
    , initialSheetsCmd : SheetsCmd msg
    }
