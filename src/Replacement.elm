module Replacement exposing (Replacement, apply, combine)

import Substring exposing (Substring)

type alias Replacement =
  { old : Substring
  , new : String
  }

apply : Replacement -> String -> String
apply replacement string =
  String.concat
    [ String.left replacement.old.i string
    , replacement.new
    , String.dropLeft
        (Substring.stop replacement.old)
        string
    ]

combine : List Replacement -> Substring -> Replacement
combine replacements source =
  case ( replacements, List.reverse replacements ) of
    ( first :: _, last :: _ ) ->
      let
        slicedSource =
          Substring.between
            first.old.i
            (Substring.stop last.old)
            source
      in
        Replacement
          slicedSource
          ( String.concat
              (combineHelp replacements slicedSource)
          )
    _ ->
      Debug.crash "Replacement.combine: No replacements"

combineHelp : List Replacement -> Substring -> List String
combineHelp replacements source =
  case replacements of
    [] ->
      [ source.s ]
    replacement :: rest ->
      let
        start = replacement.old.i
      in let
        stop = Substring.stop replacement.old
      in
        Substring.before start source ::
          replacement.new ::
            combineHelp rest (Substring.after stop source)
