module Api.Google.Exchange.SheetsCmd exposing (SheetsCmd, map, unwrap, wrap)

import Platform.Cmd



-- TYPES


type SheetsCmd a
    = SheetsCmd (String -> Platform.Cmd.Cmd a)



-- CONSTRUCTORS


wrap : (String -> Platform.Cmd.Cmd a) -> SheetsCmd a
wrap fn =
    SheetsCmd fn


unwrap : String -> SheetsCmd a -> Platform.Cmd.Cmd a
unwrap sheetId (SheetsCmd cmd) =
    cmd sheetId



-- TRANSFORMERS


map : (a -> b) -> SheetsCmd a -> SheetsCmd b
map fn (SheetsCmd cmd) =
    SheetsCmd (Platform.Cmd.map fn << cmd)
