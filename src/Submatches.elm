module Submatches exposing (submatches)

import Regex exposing (Regex)

submatches : Regex -> String -> List (Maybe String)
submatches regex s =
  List.concatMap .submatches (Regex.findAtMost 1 regex s)
