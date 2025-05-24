module Session exposing (Session, authenticate, create, isAuthed, navKey)

import Browser.Navigation as Nav



-- TYPES


type Session
    = Session
        { navKey : Nav.Key
        , isAuthed : Bool
        }



-- ACCESSORS


navKey : Session -> Nav.Key
navKey (Session session) =
    session.navKey


isAuthed : Session -> Bool
isAuthed (Session session) =
    session.isAuthed



-- TRANSFORMERS


create : Nav.Key -> Session
create key =
    Session
        { navKey = key
        , isAuthed = False
        }


authenticate : Session -> Session
authenticate (Session session) =
    Session { session | isAuthed = True }
