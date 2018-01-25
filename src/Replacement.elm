module Replacement exposing (Replacement, apply, applyAll)

import Substring exposing (Substring)

type alias Replacement =
  { old : Substring
  , new : String
  }

apply : Replacement -> String -> String
apply replacement s =
  String.concat
    [ String.left replacement.old.i s
    , replacement.new
    , String.dropLeft (Substring.stop replacement.old) s
    ]

applyAll : List Replacement -> String -> String
applyAll replacements s =
  case replacements of
    [] -> s
    replacement :: _ ->
      String.concat
        (String.left replacement.old.i s :: applyAllHelp replacements s)

applyAllHelp : List Replacement -> String -> List String
applyAllHelp replacements s =
  case replacements of
    a :: b :: rest ->
      a.new ::
        String.dropLeft (Substring.stop a.old) (String.left b.old.i s) ::
          applyAllHelp (b :: rest) s
    [ replacement ] ->
      [ replacement.new
      , String.dropLeft (Substring.stop replacement.old) s
      ]
    [] ->
      [ s ]
