module Parse exposing (Parse, init, update, song, setLowestNote, setScale)

import Chord
import Comment
import Flag
import Flags
import Highlight exposing (Highlight)
import IdChord exposing (IdChord)
import Indent
import LowestNote
import Paragraph exposing (Paragraph)
import Replacement exposing (Replacement)
import Scale exposing (Scale)
import Substring exposing (Substring)
import Suggestion exposing (Suggestion)

type alias Parse =
  { code : String
  , paragraph : Paragraph
  , highlights : List Highlight
  , suggestions : List Suggestion
  , lowestNote : Int
  , scale : Scale
  }

init : Int -> String -> Parse
init firstId code =
  initHelp (Paragraph.init firstId) code

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
          , Paragraph.highlights scale.root paragraph
          ]
    , suggestions =
        Suggestion.sort
          ( List.concat
              [ Flags.suggestions unindented
              , Paragraph.suggestions scale.root paragraph
              ]
          )
    , lowestNote = Flag.parse LowestNote.flag unindented
    , scale = scale
    }

song : Parse -> List (List (Maybe IdChord))
song parse =
  Paragraph.song parse.paragraph

setLowestNote : Int -> String -> Maybe Replacement
setLowestNote lowestNote code =
  let
    unindented =
      Indent.remove (Comment.remove (Substring 0 code))
  in
    Flag.insert LowestNote.flag lowestNote unindented

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
          offset = (scale.root - oldScale.root) % 12
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
              Just scaleReplacement
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
                  { old = Substring 0 code
                  , new =
                      Replacement.applyAll
                        (flagReplacements ++ chordReplacements)
                        code
                  }
