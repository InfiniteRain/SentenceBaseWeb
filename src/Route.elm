module Route exposing (Route(..), fromUrl, navigate, standardizeFragment)

import Browser.Navigation as Nav
import Session exposing (Session)
import Url exposing (Url)



-- ROUTING


type Route
    = Root
    | Mining
    | Batches



-- HELPERS


navigate : Session -> Route -> Cmd msg
navigate session route =
    let
        url =
            Session.currentUrl session

        newUrl =
            { url | fragment = Just <| routeToString route }
    in
    Nav.replaceUrl (Session.navKey session) (Url.toString newUrl)


fromUrl : Url -> Maybe Route
fromUrl url =
    case extractFragmentSegments url of
        [ "mining" ] ->
            Just Mining

        [ "batches" ] ->
            Just Batches

        [] ->
            Just Root

        _ ->
            Nothing


extractFragmentSegments : Url -> List String
extractFragmentSegments url =
    Maybe.withDefault "" url.fragment
        |> String.split "/"
        |> List.filter ((/=) "")


standardizeFragment : Url -> Url
standardizeFragment url =
    { url
        | fragment =
            Just (String.join "/" (extractFragmentSegments url) ++ "/")
    }


routeToString : Route -> String
routeToString route =
    "/" ++ String.join "/" (routeToSegments route)


routeToSegments : Route -> List String
routeToSegments route =
    case route of
        Root ->
            []

        Mining ->
            [ "mining" ]

        Batches ->
            [ "batches" ]
