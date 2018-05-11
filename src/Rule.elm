module Rule exposing (Fixer, Rule)

import Flag exposing (Flag)

type alias Fixer = String -> Maybe String

type alias Rule = (String, Fixer)

fromFlag : Flag -> Rule
fromFlag flag =
  ( flag.key, Maybe.map flag.code << flag.fromCode )
