module RegexExtra exposing (newLines, sentenceSplit, space, tag)

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



-- SENTENCE SPLIT


punctuation : String
punctuation =
    [ "!"
    , "\""
    , "#"
    , "$"
    , "%"
    , "&"
    , "'"
    , "("
    , ")"
    , "*"
    , "+"
    , ","
    , "\\"
    , "-"
    , "."
    , "/"
    , ":"
    , ";"
    , "<"
    , "="
    , ">"
    , "?"
    , "@"
    , "["
    , "]"
    , "^"
    , "_"
    , "`"
    , "{"
    , "|"
    , "}"
    , "~"
    , "s"
    ]
        |> List.map (\punct -> "\\" ++ punct)
        |> String.join ""


p : String
p =
    "[" ++ punctuation ++ "]"


np : String
np =
    "[^" ++ punctuation ++ "]"


sentenceRegex : Regex
sentenceRegex =
    regex
        ("(\\.{3}|"
            ++ np
            ++ "+\\-"
            ++ np
            ++ "+|"
            ++ np
            ++ "*\\'(?:"
            ++ np
            ++ "+)?|"
            ++ np
            ++ "+|"
            ++ p
            ++ ")"
        )


sentenceSplit : String -> List String
sentenceSplit text =
    Regex.find sentenceRegex text
        |> List.map .match
        |> List.filter (not << Regex.contains space)
