module Icon.Loading exposing (loadingIcon)

import Html exposing (Attribute)
import Html.Attributes exposing (attribute)
import Svg


loadingIcon : List (Attribute msg) -> Svg.Svg msg
loadingIcon attrs =
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
         , attribute "class" "animate-spin"
         ]
            ++ attrs
        )
        [ Svg.node "path" [ attribute "d" "M12 2v4" ] []
        , Svg.node "path" [ attribute "d" "m16.2 7.8 2.9-2.9" ] []
        , Svg.node "path" [ attribute "d" "M18 12h4" ] []
        , Svg.node "path" [ attribute "d" "m16.2 16.2 2.9 2.9" ] []
        , Svg.node "path" [ attribute "d" "M12 18v4" ] []
        , Svg.node "path" [ attribute "d" "m4.9 19.1 2.9-2.9" ] []
        , Svg.node "path" [ attribute "d" "M2 12h4" ] []
        , Svg.node "path" [ attribute "d" "m4.9 4.9 2.9 2.9" ] []
        ]
