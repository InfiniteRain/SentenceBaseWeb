module Basecoat exposing
    ( ariaAtomic
    , ariaControls
    , ariaCurrent
    , ariaHidden
    , ariaLabel
    , ariaLabelledBy
    , ariaOrientation
    , ariaRole
    , ariaSelected
    , classes
    , dataAlign
    , dataCategory
    , dataSide
    , dataSideBarInitialized
    , dataTooltip
    , inert
    , modalDialog
    , role
    , tabIndex
    )

import Html exposing (Attribute, Html, div, node)
import Html.Attributes exposing (attribute, classList)
import Html.Events exposing (on)
import Json.Decode as Decode



-- ATTRIBUTES


ariaControls : String -> Attribute msg
ariaControls value =
    attribute "aria-controls" value


ariaLabel : String -> Attribute msg
ariaLabel value =
    attribute "aria-label" value


ariaLabelledBy : String -> Attribute msg
ariaLabelledBy value =
    attribute "aria-labelledby" value


ariaHidden : Bool -> Attribute msg
ariaHidden value =
    attribute "aria-hidden" (boolToString value)


ariaCurrent : String -> Attribute msg
ariaCurrent value =
    attribute "aria-current" value


ariaOrientation : String -> Attribute msg
ariaOrientation value =
    attribute "aria-orientation" value


ariaRole : String -> Attribute msg
ariaRole value =
    attribute "aria-role" value


ariaSelected : Bool -> Attribute msg
ariaSelected value =
    attribute "aria-selected" (boolToString value)


ariaAtomic : Bool -> Attribute msg
ariaAtomic value =
    attribute "aria-atomic" (boolToString value)


dataAlign : String -> Attribute msg
dataAlign value =
    attribute "data-align" value


dataSide : String -> Attribute msg
dataSide value =
    attribute "data-side" value


dataTooltip : String -> Attribute msg
dataTooltip value =
    attribute "data-tooltip" value


dataSideBarInitialized : Bool -> Attribute msg
dataSideBarInitialized value =
    attribute "data-sidebar-initialized" (boolToString value)


dataCategory : String -> Attribute msg
dataCategory value =
    attribute "data-category" value


role : String -> Attribute msg
role value =
    attribute "role" value


inert : String -> Attribute msg
inert value =
    attribute "inert" value


tabIndex : String -> Attribute msg
tabIndex value =
    attribute "tabindex" value


classes : List String -> Attribute msg
classes =
    List.map (\x -> ( x, True ))
        >> classList



-- DIALOG


onCloseAttempt : msg -> Attribute msg
onCloseAttempt message =
    on "close-attempt" (Decode.succeed message)


modalDialog : Bool -> msg -> List (Attribute msg) -> List (Html msg) -> Html msg
modalDialog isOpen msg attributes children =
    node "modal-dialog"
        [ attribute "open" (boolToString isOpen)
        , onCloseAttempt msg
        ]
        [ node "dialog"
            attributes
            (div
                [ classes
                    [ "opacity-0"
                    , "absolute"
                    , "top-0"
                    , "left-0"
                    , "w-full"
                    , "h-full"
                    , "-z-10"
                    ]
                ]
                []
                :: children
            )
        ]



-- HELPERS


boolToString : Bool -> String
boolToString value =
    if value then
        "true"

    else
        "false"
