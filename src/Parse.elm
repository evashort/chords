module Parse exposing (Parse, init, update, setLowestNote, transpose)

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
import Substring exposing (Substring)
import Suggestion exposing (Suggestion)

type alias Parse =
  { code : String
  , unindented : List Substring
  , paragraph : Paragraph
  , highlights : List Highlight
  , suggestions : List Suggestion
  , scale : Scale
  , lowestNote : Int
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
  in
    { code = code
    , unindented = unindented
    , paragraph = paragraph
    , highlights =
        List.concat
          [ Comment.highlights (Substring 0 code)
          , Indent.highlights lines
          , Flags.highlights unindented
          , Paragraph.highlights paragraph
          ]
    , suggestions =
        Suggestion.sort
          ( List.concat
              [ Flags.suggestions unindented
              , Paragraph.suggestions paragraph
              ]
          )
    , scale = Flag.parse Scale.flag unindented
    , lowestNote = Flag.parse LowestNote.flag unindented
    }

setLowestNote : Int -> Parse -> Maybe Replacement
setLowestNote lowestNote parse =
  Flag.insert LowestNote.flag lowestNote parse.unindented

transpose : Int -> Parse -> Replacement
transpose offset parse =
  let
    flagReplacements =
      case
        List.filterMap
          identity
          [ Flag.insert
              LowestNote.flag
              (parse.lowestNote + offset)
              parse.unindented
          , Flag.insert
              Scale.flag
              (Scale.transpose offset parse.scale)
              parse.unindented
          ]
      of
        [ x, y ] ->
          if y.old.i < x.old.i then
            [ y, x ]
          else
            [ x, y ]
        z ->
          z
  in let
    chordReplacements =
      Paragraph.mapChords
        (Chord.transpose offset)
        parse.paragraph
  in
    { old = Substring 0 parse.code
    , new =
        Replacement.applyAll
          (flagReplacements ++ chordReplacements)
          parse.code
    }
