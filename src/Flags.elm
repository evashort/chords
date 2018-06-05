module Flags exposing (highlights, suggestions)

import Bpm
import Highlight exposing (Highlight)
import LowestNote
import Rule
import Rulebook exposing (Rulebook)
import Scale
import Substring exposing (Substring)
import Suggestion exposing (Suggestion)

book : Rulebook
book =
  Rulebook.init
    [ Rule.fromFlag Bpm.flag
    , Rule.fromFlag LowestNote.flag
    , Rule.fromFlag Scale.flag
    ]

highlights : List Substring -> List Highlight
highlights = Rulebook.highlights book

suggestions : List Substring -> List Suggestion
suggestions = Rulebook.suggestions book
