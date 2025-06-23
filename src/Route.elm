module Route exposing (Route(..), fromUrl, navigate)

import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, s)



-- ROUTING


type Route
    = Auth
    | Mining
    | PendingSentences
    | Batches


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Auth Parser.top
        , Parser.map Mining (s "mining")
        , Parser.map PendingSentences (s "pendingSentences")
        , Parser.map Batches (s "batches")
        ]



-- HELPERS


navigate : Nav.Key -> Route -> Cmd msg
navigate key route =
    Nav.replaceUrl key <| routeToString route


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url


routeToString : Route -> String
routeToString route =
    "/" ++ String.join "/" (routeToSegments route)


routeToSegments : Route -> List String
routeToSegments route =
    case route of
        Auth ->
            []

        Mining ->
            [ "mining" ]

        PendingSentences ->
            [ "pendingSentences" ]

        Batches ->
            [ "batches" ]
