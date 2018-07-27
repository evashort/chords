module Parse exposing
  (Parse, init, update, song, setBpm, setScale, setLowest)

import Bpm
import Chord
import Comment
import Flag
import Flags
import Highlight exposing (Highlight)
import Indent
import Lowest
import Paragraph exposing (Paragraph)
import Replacement exposing (Replacement)
import Scale exposing (Scale)
import Song exposing (Song)
import Substring exposing (Substring)
import Suggestion exposing (Suggestion)

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
  in let
    unindented = Indent.remove lines
  in let
    paragraph =
      Paragraph.update
        (Flag.remove unindented)
        parse.paragraph
    bpm = Flag.parse Bpm.flag unindented
    scale = Flag.parse Scale.flag unindented
    lowest = Flag.parse Lowest.flag unindented
  in
    { code = code
    , paragraph = paragraph
    , highlights =
        List.concat
          [ Comment.highlights (Substring 0 code)
          , Indent.highlights lines
          , Flags.highlights unindented
          , Paragraph.highlights scale.tonic paragraph
          ]
    , suggestions =
        Suggestion.sort
          ( List.concat
              [ Flags.suggestions unindented
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

setBpm : Maybe Float -> String -> Maybe Replacement
setBpm bpm code =
  let
    unindented =
      Indent.remove (Comment.remove (Substring 0 code))
  in
    Maybe.map
      (glue code)
      (Flag.insert Bpm.flag bpm unindented)

setScale : Scale -> String -> Maybe Replacement
setScale scale code =
  let
    unindented =
      Indent.remove (Comment.remove (Substring 0 code))
  in
    case Flag.insert Scale.flag scale unindented of
      Nothing ->
        Nothing
      Just scaleReplacement ->
        let
          oldScale = Flag.parse Scale.flag unindented
        in let
          offset = (scale.tonic - oldScale.tonic) % 12
        in
          if offset == 0 then
            Just (glue code scaleReplacement)
          else
            let
              chordReplacements =
                Paragraph.mapChords
                  (Chord.transpose offset)
                  (Flag.remove unindented)
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
    unindented =
      Indent.remove (Comment.remove (Substring 0 code))
  in
    Maybe.map
      (glue code)
      (Flag.insert Lowest.flag lowest unindented)

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
    Debug.crash
      ("Parse.glue: Replacement out of bounds: " ++ toString replacement)
