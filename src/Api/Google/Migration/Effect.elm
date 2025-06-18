module Api.Google.Migration.Effect exposing
    ( Effect
    , EffectInner(..)
    , EffectWithPayload
    , done
    , doneWithPayload
    , fail
    , sheetsTask
    )

import Api.Google.Exchange.SheetsCmd exposing (SheetsCmd)
import Api.Google.Exchange.Task as Task exposing (Error)



-- TYPES


type EffectInner msg payload
    = SheetsTask (SheetsCmd msg)
    | Fail Task.Error
    | Done payload


type alias Effect msg =
    EffectInner msg ()


type alias EffectWithPayload msg payload =
    EffectInner msg payload



-- CONSTRUCTORS


sheetsTask :
    (Result Error a -> rootMsg)
    -> Task.SheetsTask a
    -> EffectInner rootMsg payload
sheetsTask toMsg task =
    SheetsTask <| Task.sheetsAttempt toMsg task


fail : Task.Error -> EffectInner rootMsg payload
fail err =
    Fail err


done : EffectInner rootMsg ()
done =
    Done ()


doneWithPayload : payload -> EffectInner rootMsg payload
doneWithPayload payload =
    Done payload
