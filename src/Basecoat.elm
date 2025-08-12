module Basecoat exposing
    ( ariaControls
    , ariaCurrent
    , ariaHidden
    , ariaLabel
    , ariaLabelledBy
    , ariaOrientation
    , ariaRole
    , ariaSelected
    , classes
    , dataAlign
    , dataSide
    , dataSideBarInitialized
    , dataTooltip
    , inert
    , role
    , tabIndex
    )

import Html exposing (Attribute)
import Html.Attributes exposing (attribute, classList)



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



-- HELPERS


boolToString : Bool -> String
boolToString value =
    if value then
        "true"

    else
        "false"
