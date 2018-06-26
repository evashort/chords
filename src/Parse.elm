module Parse exposing
  (Parse, init, update, song, setBpm, setLowestNote, setScale)

import Bpm
import Chord
import Comment
import Flag
import Flags
import Highlight exposing (Highlight)
import Indent
import LowestNote
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
  , bpm : Float
  , lowestNote : Int
  , scale : Scale
  }

init : String -> Parse
init code =
  initHelp Paragraph.init code

update : String -> Parse -> Parse
update code parse =
  initHelp ((flip Paragraph.update) parse.paragraph) code

initHelp : (List Substring -> Paragraph) -> String -> Parse
initHelp getParagraph code =
  let
    lines = Comment.remove (Substring 0 code)
  in let
    unindented = Indent.remove lines
  in let
    paragraph = getParagraph (Flag.remove unindented)
  in let
    scale = Flag.parse Scale.flag unindented
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
    , bpm = Flag.parse Bpm.flag unindented
    , lowestNote = Flag.parse LowestNote.flag unindented
    , scale = scale
    }

song : Parse -> Song
song parse =
  Paragraph.song parse.paragraph

setBpm : Float -> String -> Maybe Replacement
setBpm bpm code =
  let
    unindented =
      Indent.remove (Comment.remove (Substring 0 code))
  in
    Maybe.map
      (glue code)
      (Flag.insert Bpm.flag bpm unindented)

setLowestNote : Int -> String -> Maybe Replacement
setLowestNote lowestNote code =
  let
    unindented =
      Indent.remove (Comment.remove (Substring 0 code))
  in
    Maybe.map
      (glue code)
      (Flag.insert LowestNote.flag lowestNote unindented)

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
        in let
          oldLowestNote =
            Flag.parse LowestNote.flag unindented
        in let
          lowestNote =
            (oldLowestNote + offset - 39) % 12 + 39
        in
          case
            Flag.insert LowestNote.flag lowestNote unindented
          of
            Nothing ->
              Just (glue code scaleReplacement)
            Just lowestNoteReplacement ->
              let
                flagReplacements =
                  if scaleReplacement.old.i < lowestNoteReplacement.old.i then
                    [ scaleReplacement, lowestNoteReplacement ]
                  else
                    [ lowestNoteReplacement, scaleReplacement ]
              in let
                chordReplacements =
                  Paragraph.mapChords
                    (Chord.transpose offset)
                    (Flag.remove unindented)
              in
                Just
                  ( glue
                      code
                      ( Replacement.combine
                          (flagReplacements ++ chordReplacements)
                          (Substring 0 code)
                      )
                  )

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
  else
    Debug.crash
      ("Parse.glue: Replacement out of bounds: " ++ toString replacement)
