module Api.Google.Migration.Effect exposing
    ( Effect
    , EffectInner(..)
    , EffectWithPayload
    , done
    , doneWithPayload
    , fail
    , task
    )

import Api.Google.TaskCmd as TaskCmd exposing (TaskCmd)
import Http
import Platform exposing (Task)



-- TYPES


type EffectInner msg payload
    = Task (TaskCmd msg)
    | Fail Http.Error
    | Done payload


type alias Effect msg =
    EffectInner msg ()


type alias EffectWithPayload msg payload =
    EffectInner msg payload



-- CONSTRUCTORS


task :
    (Result error response -> rootMsg)
    -> Task error response
    -> EffectInner rootMsg payload
task toMsg subTask =
    Task <| TaskCmd.attempt toMsg subTask


fail : Http.Error -> EffectInner rootMsg payload
fail err =
    Fail err


done : EffectInner rootMsg ()
done =
    Done ()


doneWithPayload : payload -> EffectInner rootMsg payload
doneWithPayload payload =
    Done payload
