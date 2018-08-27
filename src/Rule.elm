module Rule exposing (Fixer, Rule, fromFlag)

import Flag exposing (Flag)

type alias Fixer = String -> Maybe String

type alias Rule = (String, Fixer)

fromFlag : Flag a -> Rule
fromFlag flag =
  ( flag.key, Maybe.map flag.toCode << flag.fromCode )
