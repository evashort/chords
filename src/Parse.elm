module Parse exposing
  (Parse, init, update, song, chords, setBpm, setScale, setLowest, addWord)

import Bpm
import Chord exposing (Chord)
import Comment
import Flag
import Flags
import Highlight exposing (Highlight)
import Lowest
import Paragraph exposing (Paragraph)
import Replacement exposing (Replacement)
import Scale exposing (Scale)
import Song exposing (Song)
import Substring exposing (Substring)
import Suggestion exposing (Suggestion)

import Regex exposing (Regex)

type alias Parse =
  { code : String
  , paragraph : Paragraph
  , highlights : List Highlight
  , suggestions : List Suggestion
  , bpm : Maybe Float
  , scale : Scale
  , lowest : Maybe Int
  , defaultTitle : String
  }

init : String -> Parse
init code =
  update code initHelp

initHelp : Parse
initHelp =
  { code = ""
  , paragraph = Paragraph.init []
  , highlights = []
  , suggestions = []
  , bpm = .default Bpm.flag
  , scale = .default Scale.flag
  , lowest = .default Lowest.flag
  , defaultTitle = "Untitled"
  }

update : String -> Parse -> Parse
update code parse =
  let
    lines = Comment.remove (Substring 0 code)
  in
  let
    paragraph =
      Paragraph.update
        (Flag.remove lines)
        parse.paragraph
    bpm = Flag.parse Bpm.flag lines
    scale = Flag.parse Scale.flag lines
    lowest = Flag.parse Lowest.flag lines
  in
    { code = code
    , paragraph = paragraph
    , highlights =
        List.concat
          [ Comment.highlights (Substring 0 code)
          , Flags.highlights lines
          , Paragraph.highlights scale.tonic paragraph
          ]
    , suggestions =
        Suggestion.sort
          ( List.concat
              [ Flags.suggestions lines
              , Paragraph.suggestions scale.tonic paragraph
              ]
          )
    , bpm =
        if bpm == parse.bpm then
          parse.bpm
        else
          bpm
    , scale =
        if scale == parse.scale then
          parse.scale
        else
          scale
    , lowest =
        if lowest == parse.lowest then
          parse.lowest
        else
          lowest
    , defaultTitle =
        case Paragraph.defaultTitle paragraph of
          "" ->
            "Untitled"
          title ->
            title
    }

song : Parse -> Song
song parse =
  Paragraph.song parse.paragraph

chords : Parse -> List Chord
chords parse =
  Paragraph.chords parse.paragraph

setBpm : Maybe Float -> String -> Maybe Replacement
setBpm bpm code =
  let
    lines = Comment.remove (Substring 0 code)
  in
    Maybe.map
      (glue code)
      (Flag.insert Bpm.flag bpm lines)

setScale : Scale -> String -> Maybe Replacement
setScale scale code =
  let
    lines = Comment.remove (Substring 0 code)
  in
    case Flag.insert Scale.flag scale lines of
      Nothing ->
        Nothing
      Just scaleReplacement ->
        let
          oldScale = Flag.parse Scale.flag lines
        in
        let
          offset = modBy 12 (scale.tonic - oldScale.tonic)
        in
          if offset == 0 then
            Just (glue code scaleReplacement)
          else
            let
              chordReplacements =
                Paragraph.mapChords
                  (Chord.transpose offset)
                  (Flag.remove lines)
            in
              Just
                ( glue
                    code
                    ( Replacement.combine
                        (scaleReplacement :: chordReplacements)
                        (Substring 0 code)
                    )
                )

setLowest : Maybe Int -> String -> Maybe Replacement
setLowest lowest code =
  let
    lines = Comment.remove (Substring 0 code)
  in
    Maybe.map
      (glue code)
      (Flag.insert Lowest.flag lowest lines)

glue : String -> Replacement -> Replacement
glue source replacement =
  if
    replacement.old.i >= 0 &&
      Substring.stop replacement.old <= String.length source
  then
    replacement
  else if
    replacement.old.i == String.length source + 1 &&
      replacement.old.s == ""
  then
    Replacement
      (Substring (replacement.old.i - 1) "")
      ("\n" ++ replacement.new)
  else if
    replacement.old.i >= 0 &&
      Substring.stop replacement.old == String.length source + 1 &&
      String.endsWith "\n" replacement.old.s
  then
    { replacement
    | old = Substring.dropRight 1 replacement.old
    }
  else
    Debug.todo
      ( "Parse.glue: Replacement out of bounds: " ++
          Debug.toString replacement
      )

addWord : String -> Parse -> Replacement
addWord word parse =
  let
    prefix =
      if String.endsWith "\n" parse.code then
        ""
      else
        case Paragraph.lastWordEnd parse.paragraph of
          Nothing ->
            if String.isEmpty parse.code then
              ""
            else
              "\n"
          Just lastWordEnd ->
            if lastWordEnd == String.length parse.code then
              " "
            else if
              Regex.contains
                onlySpaces
                (String.dropLeft lastWordEnd parse.code)
            then
              ""
            else
              "\n"
  in
    Replacement
      { i = String.length parse.code, s = "" }
      (prefix ++ word)

onlySpaces : Regex
onlySpaces =
  Maybe.withDefault Regex.never (Regex.fromString "^ +$")
