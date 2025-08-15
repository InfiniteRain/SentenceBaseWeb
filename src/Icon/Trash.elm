module Icon.Trash exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (attribute)
import Svg


trashIcon : List (Attribute msg) -> Svg.Svg msg
trashIcon attrs =
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
        [ Svg.node "path"
            [ attribute "d" "M3 6h18"
            ]
            []
        , Svg.node "path"
            [ attribute "d" "M19 6v14c0 1-1 2-2 2H7c-1 0-2-1-2-2V6"
            ]
            []
        , Svg.node "path"
            [ attribute "d" "M8 6V4c0-1 1-2 2-2h4c1 0 2 1 2 2v2"
            ]
            []
        ]

