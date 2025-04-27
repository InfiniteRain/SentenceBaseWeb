module Api.Google.Migration.Config exposing (Config)

import Api.Google.TaskCmd exposing (TaskCmd)


type alias Config model msg =
    { id : String
    , model : model
    , initialTask : TaskCmd msg
    }
