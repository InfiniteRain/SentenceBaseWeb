module Icon.Plus exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (attribute)
import Svg


plusIcon : List (Attribute msg) -> Svg.Svg msg
plusIcon attrs =
    Svg.node "svg"
        ([ attribute "xmlns" "http://www.w3.org/2000/svg"
         , attribute "width" "28"
         , attribute "height" "28"
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
            [ attribute "d" "M5 12h14"
            ]
            []
        , Svg.node "path" [ attribute "d" "M12 5v14" ] []
        ]

