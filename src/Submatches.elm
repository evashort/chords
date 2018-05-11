module Submatches exposing (submatches)

import Regex exposing (Regex, HowMany(..))

submatches : Regex -> String -> List (Maybe String)
submatches regex s =
  List.concatMap .submatches (Regex.find (AtMost 1) regex s)
