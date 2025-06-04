module Api.Uuid exposing (Model, Msg(..), init, subscriptions, update)

import OutMsg exposing (OutMsg)
import UUID as Uuid



-- MODEL


type alias Model =
    Uuid.Seeds


init : Uuid.Seeds -> ( Model, Cmd (Msg rootMsg) )
init seeds =
    ( seeds, Cmd.none )



-- UPDATE


type Msg rootMsg
    = SentRequest (String -> rootMsg)


update : Msg rootMsg -> Model -> ( Model, Cmd (Msg rootMsg), OutMsg rootMsg )
update (SentRequest toMsg) model =
    let
        ( uuid, newSeeds ) =
            Uuid.step model
    in
    ( newSeeds, Cmd.none, OutMsg.some <| toMsg <| Uuid.toString uuid )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub (Msg rootMsg)
subscriptions _ =
    Sub.none
