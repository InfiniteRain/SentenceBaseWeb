module Icon.SquarePen exposing (squarePenIcon)

import Html exposing (Attribute)
import Html.Attributes exposing (attribute)
import Svg


squarePenIcon : List (Attribute msg) -> Svg.Svg msg
squarePenIcon attrs =
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
         , attribute "style" "overflow: visible;"
         ]
            ++ attrs
        )
        [ Svg.node "path"
            [ attribute "d" "M12 3H5a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h14a2 2 0 0 0 2-2v-7"
            ]
            []
        , Svg.node "path"
            [ attribute "d" "M18.375 2.625a1 1 0 0 1 3 3l-9.013 9.014a2 2 0 0 1-.853.505l-2.873.84a.5.5 0 0 1-.62-.62l.84-2.873a2 2 0 0 1 .506-.852z"
            , attribute "style" "transform: none; transform-origin: 50% 50% 0px; transform-box: fill-box;"
            ]
            []
        ]
