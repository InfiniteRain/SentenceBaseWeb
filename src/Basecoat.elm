module Basecoat exposing
    ( ariaControls
    , ariaCurrent
    , ariaHidden
    , ariaLabel
    , ariaLabelledBy
    , dataAlign
    , dataSide
    , dataSideBarInitialized
    , dataTooltip
    , inert
    , role
    , staticClasses
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


staticClasses : List String -> Attribute msg
staticClasses =
    List.map (\x -> ( x, True ))
        >> classList



-- HELPERS


boolToString : Bool -> String
boolToString value =
    if value then
        "true"

    else
        "false"
