module Paragraph exposing
  (Paragraph, init, update, highlights, song, suggestions, mapChords)

import Highlight exposing (Highlight)
import IdChord exposing (IdChord)
import Replacement exposing (Replacement)
import Substring exposing (Substring)
import Suggestion exposing (Suggestion)
import Train exposing (Train)
import Word exposing (Word)
import Zipper

import Regex exposing (Regex, HowMany(..))
import Set exposing (Set)

type alias Paragraph =
  { nextId : Int
  , words : Train Word
  }

init : Int -> List Substring -> Paragraph
init firstId lines =
  let substrings = split lines in
    { nextId = firstId + Train.length substrings
    , words =
        Train.indexedMap (Word.init << (+) firstId) substrings
    }

update : List Substring -> Paragraph -> Paragraph
update lines { nextId, words } =
  let
    doubleZipped =
      Zipper.doubleZip Word.update (split lines) words
  in
    { nextId =
        nextId + Train.length doubleZipped.upper
    , words =
        List.concat
          [ doubleZipped.left
          , Train.indexedMap
              (Word.init << (+) nextId)
              doubleZipped.upper
          , doubleZipped.right
          ]
    }

split : List Substring -> Train Substring
split lines =
  Train.fromCars (List.map (Substring.find All wordRegex) lines)

highlights : Int -> Paragraph -> List Highlight
highlights key paragraph =
  List.filterMap (Word.highlight key) paragraph.words

song : Paragraph -> List (List (Maybe IdChord))
song paragraph =
  List.filter
    (not << List.isEmpty)
    (Train.cars (Train.filterMap Word.meaning))

suggestions : Int -> Paragraph -> List Suggestion
suggestions key paragraph =
  Suggestion.groupByReplacement
    ( List.filterMap
        (Word.suggestion key)
        (Train.flatten paragraph.words)
    )

mapChords : (Chord -> Chord) -> List Substring -> List Replacement
mapChords f lines =
  List.filterMap
    (Word.mapChord f)
    (List.concatMap (Substring.find All wordRegex) lines)
