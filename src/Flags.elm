module Flags exposing (highlights, suggestions)

import Highlight exposing (Highlight)
import LowestNote
import Rulebook exposing (Rulebook)
import Scale
import Suggestion exposing (Suggestion)

book : Rulebook
book =
  Rulebook.init
    [ LowestNote.rule
    , Scale.rule
    ]

highlights : List Substring -> List Highlight
highlights = Rulebook.highlights book

suggestions : List Substring -> List Suggestion
suggestions = Rulebook.suggestions book
