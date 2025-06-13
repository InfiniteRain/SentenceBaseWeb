module Api.Google.ListConstructor exposing
    ( ListConstructor
    , cellBoolValue
    , cellFormulaValue
    , cellNumberValue
    , cellStringValue
    , constructFromList
    , extract
    , field
    )

import Api.Google.Requests as Requests exposing (SheetResponseCellExtendedData(..))



-- TYPES


type ListConstructor elem constr
    = ListConstructor
        { list : List elem
        , maybeConstructor : Maybe constr
        }



-- CONSTRUCTORS


constructFromList : constr -> List elem -> ListConstructor elem constr
constructFromList constr list =
    ListConstructor
        { list = list
        , maybeConstructor = Just constr
        }



-- TRANSFORMERS


field :
    (elem -> Maybe a)
    -> ListConstructor elem (a -> b)
    -> ListConstructor elem b
field map (ListConstructor { list, maybeConstructor }) =
    case ( list, maybeConstructor ) of
        ( head :: rest, Just constructor ) ->
            ListConstructor
                { list = rest
                , maybeConstructor =
                    map head
                        |> Maybe.map constructor
                }

        _ ->
            ListConstructor
                { list = list
                , maybeConstructor = Nothing
                }



-- ACCESSORS


extract : ListConstructor elem constr -> Maybe constr
extract (ListConstructor { maybeConstructor }) =
    maybeConstructor



-- HELPERS


cellNumberValue : Requests.SheetResponseCellData -> Maybe Float
cellNumberValue { effectiveValue } =
    case effectiveValue of
        Just (Number float) ->
            Just float

        _ ->
            Nothing


cellStringValue : Requests.SheetResponseCellData -> Maybe String
cellStringValue { effectiveValue } =
    case effectiveValue of
        Just (String string) ->
            Just string

        _ ->
            Nothing


cellBoolValue : Requests.SheetResponseCellData -> Maybe Bool
cellBoolValue { effectiveValue } =
    case effectiveValue of
        Just (Bool bool) ->
            Just bool

        _ ->
            Nothing


cellFormulaValue : Requests.SheetResponseCellData -> Maybe String
cellFormulaValue { effectiveValue } =
    case effectiveValue of
        Just (Formula formula) ->
            Just formula

        _ ->
            Nothing
