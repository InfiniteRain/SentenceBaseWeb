module Icon.WarningCircle exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (attribute)
import Svg


warningCircleIcon : List (Attribute msg) -> Svg.Svg msg
warningCircleIcon attrs =
    Svg.node "svg"
        ([ attribute "xmlns" "http://www.w3.org/2000/svg"
         , attribute "width" "24"
         , attribute "height" "24"
         , attribute "viewBox" "0 0 24 24"
         , attribute "fill" "none"
         , attribute "stroke" "currentColor"
         , attribute "stroke-width" "2"
         , attribute "stroke-linecap" "round"
         , attribute "stroke-linejoin" "round"
         ]
            ++ attrs
        )
        [ Svg.node "circle"
            [ attribute "cx" "12"
            , attribute "cy" "12"
            , attribute "r" "10"
            ]
            []
        , Svg.node "line"
            [ attribute "x1" "12"
            , attribute "x2" "12"
            , attribute "y1" "8"
            , attribute "y2" "12"
            ]
            []
        , Svg.node "line"
            [ attribute "x1" "12"
            , attribute "x2" "12.01"
            , attribute "y1" "16"
            , attribute "y2" "16"
            ]
            []
        ]
