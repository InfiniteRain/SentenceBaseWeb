module Api.Uuid exposing
    ( Model
    , Msg(..)
    , init
    , subscriptions
    , update
    )

import Api.OutMsg as OutMsg exposing (OutMsg)
import Effect exposing (Effect, UuidAction(..))
import UUID as Uuid



-- MODEL


type alias Model =
    Uuid.Seeds


init : Uuid.Seeds -> ( Model, Cmd (Msg rootMsg) )
init seeds =
    ( seeds, Cmd.none )



-- UPDATE


type Msg rootMsg
    = SentRequest (Effect rootMsg) (UuidAction rootMsg)


update : Msg rootMsg -> Model -> ( Model, Cmd (Msg rootMsg), OutMsg rootMsg )
update (SentRequest effect action) model =
    case action of
        SingleUuid toMsg ->
            let
                ( uuid, newSeeds ) =
                    Uuid.step model
            in
            ( newSeeds
            , Cmd.none
            , OutMsg.some effect (toMsg <| Uuid.toString uuid)
            )

        MultipleUuids num toMsg ->
            let
                ( uuids, newSeeds ) =
                    multiple num model []
            in
            ( newSeeds
            , Cmd.none
            , OutMsg.some effect (toMsg uuids)
            )


multiple : Int -> Uuid.Seeds -> List String -> ( List String, Uuid.Seeds )
multiple num seeds final =
    if num <= 0 then
        ( final, seeds )

    else
        let
            ( uuid, newSeeds ) =
                Uuid.step seeds
        in
        multiple (num - 1) newSeeds (Uuid.toString uuid :: final)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub (Msg rootMsg)
subscriptions _ =
    Sub.none
