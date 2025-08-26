module Icon.Cross exposing (crossIcon)

import Html exposing (Attribute)
import Html.Attributes exposing (attribute)
import Svg


crossIcon : List (Attribute msg) -> Svg.Svg msg
crossIcon attrs =
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
         , attribute "class" "lucide lucide-x-icon lucide-x"
         ]
            ++ attrs
        )
        [ Svg.node "path"
            [ attribute "d" "M18 6 6 18" ]
            []
        , Svg.node "path" [ attribute "d" "m6 6 12 12" ] []
        ]
