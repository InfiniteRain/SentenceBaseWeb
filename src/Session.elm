module Session exposing
    ( Session
    , authenticate
    , create
    , currentUrl
    , isAuthed
    , navKey
    , replaceUrl
    )

import Browser.Navigation as Nav
import Url exposing (Url)



-- TYPES


type Session
    = Session
        { navKey : Nav.Key
        , currentUrl : Url
        , isAuthed : Bool
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



-- TRANSFORMERS


create : Nav.Key -> Url -> Session
create key url =
    Session
        { navKey = key
        , isAuthed = False
        , currentUrl = url
        }


replaceUrl : Url -> Session -> Session
replaceUrl url (Session session) =
    Session { session | currentUrl = url }


authenticate : Session -> Session
authenticate (Session session) =
    Session { session | isAuthed = True }
