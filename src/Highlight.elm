module Highlight exposing (Highlight, toSwatches)

import Swatch exposing (Swatch)
import Substring exposing (Substring)

type alias Highlight =
  { fg : String
  , bg : String
  , substring : Substring
  }

toSwatches : String -> List Highlight -> List Swatch
toSwatches source highlights =
  toSwatchesHelp
    (Substring 0 source)
    (List.sortBy (.i << .substring) highlights)

toSwatchesHelp : Substring -> List Highlight -> List Swatch
toSwatchesHelp source highlights =
  case highlights of
    [] ->
      if source.s == "" then
        []
      else
        [ Swatch.plain source.s ]
    highlight :: rest ->
      let
        start = highlight.substring.i
        stop = Substring.stop highlight.substring
      in let
        before = Substring.before start source
        after = Substring.after stop source
      in
        if before == "" then
          toSwatch highlight :: toSwatchesHelp after rest
        else
          Swatch.plain before ::
            toSwatch highlight :: toSwatchesHelp after rest

toSwatch : Highlight -> Swatch
toSwatch { fg, bg, substring } =
  Swatch fg bg substring.s
