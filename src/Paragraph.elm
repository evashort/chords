module Paragraph exposing
  (Paragraph, init, update, highlights, song, suggestions, transpose)

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
  let substrings = Train.fromCars (parse lines) in
    { nextId = firstId + Train.length substrings
    , words =
        Train.indexedMap (Word.init << (+) firstId) substrings
    }

update : List Substring -> Paragraph -> Paragraph
update substrings { nextId, words } =
  let
    doubleZipped =
      Zipper.doubleZip Word.update substrings words
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

transpose : Int -> Paragraph -> List Replacement
transpose offset paragraph =
  List.filterMap
    (Word.transpose offset)
    (Train.flatten paragraph.words)

parse : List Substring -> List (List Substring)
parse lines =
  let
    relevantLines =
      List.reverse (takeWhile relevant (List.reverse lines))
  in
    List.map (Substring.find All wordRegex) relevantLines

wordRegex : Regex
wordRegex = Regex.regex "[^ ]+"

firstWord : Regex
firstWord = Regex.regex "^[^ ]*"

relevant : Substring -> Bool
relevant line =
  case Regex.find (AtMost 1) firstWord line.s of
    [] ->
      False
    match :: _ ->
      not (Set.member match.match badWords)

badWords : Set String
badWords =
  Set.fromList [ "key:", "octave:" ]

takeWhile : (a -> Bool) -> List a -> List a
takeWhile condition xs =
  case xs of
    [] ->
      []
    x :: rest ->
      if condition x then
        x :: takeWhile condition rest
      else
        []
