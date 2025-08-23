module Session exposing
    ( Session
    , authenticate
    , create
    , currentUrl
    , isAuthed
    , navKey
    , replaceUrl
    , replaceZone
    , zone
    )

import Browser.Navigation as Nav
import Time
import Url exposing (Url)



-- TYPES


type Session
    = Session
        { navKey : Nav.Key
        , currentUrl : Url
        , isAuthed : Bool
        , zone : Time.Zone
        }



-- ACCESSORS


navKey : Session -> Nav.Key
navKey (Session session) =
    session.navKey


currentUrl : Session -> Url
currentUrl (Session session) =
    session.currentUrl


isAuthed : Session -> Bool
isAuthed (Session session) =
    session.isAuthed


zone : Session -> Time.Zone
zone (Session session) =
    session.zone



-- TRANSFORMERS


create : Nav.Key -> Url -> Time.Zone -> Session
create key url timeZone =
    Session
        { navKey = key
        , isAuthed = False
        , currentUrl = url
        , zone = timeZone
        }


replaceUrl : Url -> Session -> Session
replaceUrl url (Session session) =
    Session { session | currentUrl = url }


authenticate : Session -> Session
authenticate (Session session) =
    Session { session | isAuthed = True }


replaceZone : Time.Zone -> Session -> Session
replaceZone timeZone (Session session) =
    Session { session | zone = timeZone }
