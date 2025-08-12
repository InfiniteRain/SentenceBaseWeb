module Icon.Menu exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (attribute)
import Svg


menuIcon : List (Attribute msg) -> Svg.Svg msg
menuIcon attrs =
    Svg.node "svg"
        ([ attribute "xmlns" "http://www.w3.org/2000/svg"
         , attribute "xmlns:xlink" "http://www.w3.org/1999/xlink"
         , attribute "width" "24"
         , attribute "height" "24"
         , attribute "viewBox" "0 0 24 24"
         , attribute "fill" "none"
         , attribute "stroke" "currentColor"
         , attribute "stroke-width" "2"
         , attribute "stroke-linecap" "round"
         , attribute "stroke-linejoin" "round"
         , attribute "data-darkreader-inline-stroke" ""
         , attribute "style" "--darkreader-inline-stroke: currentColor;"
         ]
            ++ attrs
        )
        [ Svg.node "rect"
            [ attribute "width" "18"
            , attribute "height" "18"
            , attribute "x" "3"
            , attribute "y" "3"
            , attribute "rx" "2"
            ]
            []
        , Svg.node "path" [ attribute "d" "M9 3v18" ] []
        ]
