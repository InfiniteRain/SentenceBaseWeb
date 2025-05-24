module RegexExtra exposing (newLines, space, tag)

import Regex exposing (Regex)



-- HELPERS


regex : String -> Regex
regex str =
    Regex.fromString str
        |> Maybe.withDefault Regex.never


space : Regex
space =
    regex "\\s+"


newLines : Regex
newLines =
    regex "\\n+"


tag : Regex
tag =
    regex "<[^>]*>"
