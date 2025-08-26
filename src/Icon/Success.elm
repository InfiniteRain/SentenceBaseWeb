module Icon.Success exposing (successIcon)

import Html exposing (Attribute)
import Html.Attributes exposing (attribute)
import Svg


successIcon : List (Attribute msg) -> Svg.Svg msg
successIcon attrs =
    Svg.node "svg"
        ([ attribute "aria-hidden" "true"
         , attribute "xmlns" "http://www.w3.org/2000/svg"
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
        , Svg.node "path" [ attribute "d" "m9 12 2 2 4-4" ] []
        ]
