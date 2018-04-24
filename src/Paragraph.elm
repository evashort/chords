module Paragraph exposing
  (Paragraph, init, update, view, song, suggestions, transpose)

import Highlight exposing (Highlight)
import IdChord exposing (IdChord)
import Replacement exposing (Replacement)
import Substring exposing (Substring)
import Suggestion exposing (Suggestion)
import Word exposing (Word)
import Zipper

type alias Paragraph =
  { nextId : Int
  , words : List Word
  }

init : Int -> List Substring -> Paragraph
init firstId substrings =
  { nextId = firstId + List.length substrings
  , words =
      List.indexedMap (Word.init << (+) firstId) substrings
  }

update : List Substring -> Paragraph -> Paragraph
update substrings paragraph =
  let
    doubleZipped =
      Zipper.doubleZip Word.update substrings paragraph.words
  in
    { nextId =
        paragraph.nextId + List.length doubleZipped.upper
    , words =
        List.concat
          [ doubleZipped.left
          , List.indexedMap
              (Word.init << (+) paragraph.nextId)
              doubleZipped.upper
          , doubleZipped.right
          ]
    }

view : Int -> Paragraph -> List Highlight
view key paragraph =
  List.filterMap (Word.view key) paragraph.words

song : Paragraph -> List (List (Maybe IdChord))
song paragraph =
  List.filter
    (not << List.isEmpty)
    ( List.map
        (List.filterMap Word.meaning)
        (splitList Word.isNewline paragraph.words)
    )

splitList : (a -> Bool) -> List a -> List (List a)
splitList pred xs =
  let ( l, ls ) = splitListHelp pred xs in
    l :: ls

splitListHelp : (a -> Bool) -> List a -> ( List a, List (List a) )
splitListHelp pred xs =
  case xs of
    x :: rest ->
      let ( l, ls ) = splitListHelp pred rest in
        if pred x then
          ( [], l :: ls )
        else
          ( x :: l, ls )
    [] ->
      ( [], [] )

suggestions : Int -> Paragraph -> List Suggestion
suggestions key paragraph =
  Suggestion.groupByReplacement
    (List.filterMap (Word.suggestion key) paragraph.words)

transpose : Int -> Paragraph -> List Replacement
transpose offset paragraph =
  List.filterMap (Word.transpose offset) paragraph.words
