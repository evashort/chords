module MainParser exposing
  (Model, init, update, view, getChords, getSuggestions)

import ChordParser exposing (IdChord)
import Flag exposing (Flag(..))
import Highlight exposing (Highlight)
import ParsedFlag exposing (ParsedFlag)
import Substring exposing (Substring)
import Suggestion exposing (Suggestion)

import Dict exposing (Dict)
import Regex exposing (Regex, HowMany(..))
import Set exposing (Set)

type alias Model =
  { chordModel : ChordParser.Model
  , flags : List ParsedFlag
  , comments : List Substring
  , indents : List Substring
  , key : Int
  , keyRange : Substring
  }

init : Int -> Substring -> Model
init firstId whole =
  let parseResult = parse whole in
    { chordModel =
        ChordParser.init firstId parseResult.words
    , flags = parseResult.flags
    , comments = parseResult.comments
    , indents = parseResult.indents
    , key = parseResult.key
    , keyRange = parseResult.keyRange
    }

update : Substring -> Model -> Model
update whole model =
  let parseResult = parse whole in
    { chordModel =
        ChordParser.update parseResult.words model.chordModel
    , flags = parseResult.flags
    , comments = parseResult.comments
    , indents = parseResult.indents
    , key = parseResult.key
    , keyRange = parseResult.keyRange
    }

view : Int -> Model -> List Highlight
view key model =
  ChordParser.view key model.chordModel ++
    List.map (Highlight "" "#008000" "#ffffff") model.comments ++
      List.map (Highlight "" "#ffffff" "#ff0000") model.indents ++
        List.concatMap
          (ParsedFlag.view (if key == model.key then key else -1))
          model.flags

getChords : Model -> List (List (Maybe IdChord))
getChords = ChordParser.getChords << .chordModel

getSuggestions :
  Int -> Model -> ( List Suggestion, Dict String (Set ( Int, Int )) )
getSuggestions key model =
  let
    flagSuggestions =
      Suggestion.groupByReplacement
        (List.filterMap ParsedFlag.getSuggestion model.flags)
  in let
    flagRangeSets =
      Dict.map (always Suggestion.rangeSet) flagSuggestions
  in let
    ( chordSuggestions, chordRangeSets ) =
      ChordParser.getSuggestions key model.chordModel
  in
    ( Suggestion.sort
        (Dict.values flagSuggestions ++ chordSuggestions)
    , Dict.merge
        Dict.insert insertUnion Dict.insert
        flagRangeSets chordRangeSets
        Dict.empty
    )

insertUnion :
  comparable1 -> Set comparable2 -> Set comparable2 ->
    Dict comparable1 (Set comparable2) -> Dict comparable1 (Set comparable2)
insertUnion k x y dict =
  Dict.insert k (Set.union x y) dict

type alias ParseResult =
  { words : List Substring
  , flags : List ParsedFlag
  , comments : List Substring
  , indents : List Substring
  , key : Int
  , keyRange : Substring
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
  in let
    canon = ParsedFlag.canon flags
  in let
    ( key, keyRange ) =
      case Dict.get "key:" canon of
        Nothing ->
          ( 0, Substring 0 "" ) -- C major is the default
        Just flag ->
          ( case ParsedFlag.officialFlag flag of
              Just (KeyFlag k) -> k
              _ -> 0
          , flag.code
          )
  in
    { words = List.concatMap .words chordArea
    , flags = flags
    , comments = List.filterMap .comment lineResults
    , indents = List.filterMap .indent lineResults
    , key = key
    , keyRange = keyRange
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
    parseLine linen

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
