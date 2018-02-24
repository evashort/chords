module MainParser exposing
  ( Model, init, update, view, getChords, getSuggestions, setKey
  , setLowestNote, transpose
  )

import ChordParser exposing (IdChord)
import Flag exposing (Flag(..))
import Highlight exposing (Highlight)
import ParsedFlag exposing (ParsedFlag)
import Replacement exposing (Replacement)
import Skin exposing (Skin)
import Substring exposing (Substring)
import Suggestion exposing (Suggestion)

import Regex exposing (Regex, HowMany(..))

type alias Model =
  { chordModel : ChordParser.Model
  , flags : List ParsedFlag
  , comments : List Substring
  , indents : List Substring
  , skin : Skin
  }

init : Int -> Substring -> Model
init firstId whole =
  let parseResult = parse whole in
    { chordModel =
        ChordParser.init firstId parseResult.skin.lowestNote parseResult.words
    , flags = parseResult.flags
    , comments = parseResult.comments
    , indents = parseResult.indents
    , skin = parseResult.skin
    }

update : Substring -> Model -> Model
update whole model =
  let parseResult = parse whole in
    { chordModel =
        ChordParser.update
          parseResult.skin.lowestNote
          parseResult.words
          model.chordModel
    , flags = parseResult.flags
    , comments = parseResult.comments
    , indents = parseResult.indents
    , skin =
        if parseResult.skin == model.skin then
          model.skin
        else
          parseResult.skin
    }

view : Model -> List Highlight
view model =
  List.concat
    [ ChordParser.view model.skin.key model.chordModel
    , List.map (Highlight "#008000" "#ffffff") model.comments
    , List.map (Highlight "#ffffff" "#ff0000") model.indents
    , List.concatMap (ParsedFlag.view model.skin) model.flags
    ]

getChords : Model -> List (List (Maybe IdChord))
getChords =
  ChordParser.getChords << .chordModel

getSuggestions : Model -> List Suggestion
getSuggestions model =
  Suggestion.sort
    ( List.concat
        [ Suggestion.groupByReplacement
            (List.filterMap ParsedFlag.getSuggestion model.flags)
        , ChordParser.getSuggestions model.skin.key model.chordModel
        ]
    )

setKey : Int -> Model -> Replacement
setKey key model =
  case
    List.reverse
      (List.filter ((==) "key:" << .s << .name) model.flags)
  of
    [] ->
      Replacement
        { i = 0, s = "" }
        ("key: " ++ Flag.codeValue (KeyFlag key) ++ "\n")
    flag :: _ ->
      Replacement
        flag.value
        (Flag.codeValue (KeyFlag key))

setLowestNote : Int -> Model -> Replacement
setLowestNote lowestNote model =
  case
    List.reverse
      (List.filter ((==) "octave:" << .s << .name) model.flags)
  of
    [] ->
      Replacement
        { i = 0, s = "" }
        ("octave: " ++ Flag.codeValue (LowestNoteFlag lowestNote) ++ "\n")
    flag :: _ ->
      Replacement
        flag.value
        (Flag.codeValue (LowestNoteFlag lowestNote))

transpose : Int -> Model -> List Replacement
transpose offset model =
  ChordParser.transpose offset model.chordModel

type alias ParseResult =
  { words : List Substring
  , flags : List ParsedFlag
  , comments : List Substring
  , indents : List Substring
  , skin : Skin
  }

parse : Substring -> ParseResult
parse whole =
  let
    lineResults =
      List.map
        parseTerminatedLine
        (Substring.find All (Regex.regex ".*\n?") whole)
  in let
    chordArea =
      case
        lastTrueAndBeyond (ParsedFlag.isOfficial << .flag) lineResults
      of
        [] -> lineResults
        _ :: beyond -> beyond
  in let
    flags = List.filterMap .flag lineResults
  in
    { words = List.concatMap .words chordArea
    , flags = flags
    , comments = List.filterMap .comment lineResults
    , indents = List.filterMap .indent lineResults
    , skin = ParsedFlag.getSkin flags
    }

lastTrueAndBeyond : (a -> Bool) -> List a -> List a
lastTrueAndBeyond pred xs =
  case xs of
    [] -> []
    x :: rest ->
      case lastTrueAndBeyond pred rest of
        [] -> if pred x then xs else []
        result -> result

type alias LineResult =
  { words : List Substring
  , flag : Maybe ParsedFlag
  , comment : Maybe Substring
  , indent : Maybe Substring
  }

parseTerminatedLine : Substring -> LineResult
parseTerminatedLine linen =
  if String.endsWith "\n" linen.s then
    let
      result = parseLine (Substring.dropRight 1 linen)
    in let
      wordsFixed =
        if List.isEmpty result.words then
          result
        else
          { result | words = result.words ++ [ Substring.right 1 linen ] }
    in
      case wordsFixed.flag of
        Nothing ->
          wordsFixed
        Just flag ->
          { wordsFixed
          | flag = Just { flag | nextLineStart = Substring.stop linen }
          }
  else
    let result = parseLine linen in
      case result.flag of
        Nothing ->
          result
        Just flag ->
          { result
          | flag = Just { flag | nextLineStart = Substring.stop linen + 1 }
          }

parseLine : Substring -> LineResult
parseLine line =
  case Substring.find (AtMost 1) (Regex.regex "^#.*") line of
    comment :: _ ->
      { words = []
      , flag = Nothing
      , comment = Just comment
      , indent = Nothing
      }
    [] ->
      let
        comment =
          case Substring.find (AtMost 1) (Regex.regex " #.*") line of
            spaceAndComment :: _ ->
              Just (Substring.dropLeft 1 spaceAndComment)
            [] ->
              Nothing
      in let
        codeAndSpace =
          case comment of
            Just c -> Substring.dropRight (String.length c.s) line
            Nothing -> line
      in let
        code =
          case
            Substring.find (AtMost 1) (Regex.regex "^.*[^ ]") codeAndSpace
          of
            x :: _ -> x
            [] -> { codeAndSpace | s = "" }
      in
        case Substring.find (AtMost 1) (Regex.regex "^ +") code of
          indent :: _ ->
            { words = []
            , flag = Nothing
            , comment = comment
            , indent = Just indent
            }
          [] ->
            let flag = ParsedFlag.fromCode code in
              { words =
                  if ParsedFlag.isOfficial flag then []
                  else Substring.find All (Regex.regex "[^ ]+") code
              , flag = flag
              , comment = comment
              , indent = Nothing
              }
