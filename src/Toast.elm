module Toast exposing (Category(..), Config)

-- TYPES


type alias Config =
    { category : Category
    , title : String
    , description : String
    }


type Category
    = Success
    | Error
    | Info
    | Warning
