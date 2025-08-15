module Icon.WarningTriangle exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (attribute)
import Svg


warningTriangleIcon : List (Attribute msg) -> Svg.Svg msg
warningTriangleIcon attrs =
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
        [ Svg.node "path"
            [ attribute "d"
                "m21.73 18-8-14a2 2 0 0 0-3.48 0l-8 14A2 2 0 0 0 4 21h16a2 2 0 0 0 1.73-3"
            ]
            []
        , Svg.node "path" [ attribute "d" "M12 9v4" ] []
        , Svg.node "path" [ attribute "d" "M12 17h.01" ] []
        ]
