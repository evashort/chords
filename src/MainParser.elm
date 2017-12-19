module MainParser exposing
  (Model, init, update, view, getChords, getSuggestions)

import ChordParser exposing (IdChord)
import Flag exposing (Flag(..))
import Highlight exposing (Highlight)
import Substring exposing (Substring)
import Suggestion exposing (Suggestion)
import Swatch exposing (Swatch)

import Dict exposing (Dict)
import Regex exposing (Regex, HowMany(..), Match)
import Set exposing (Set)

type alias Model =
  { chordModel : ChordParser.Model
  , flags : List ParsedFlag
  , comments : List Substring
  , indents : List Substring
  , key : Int
  , lineAfterKey : Int
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
    , lineAfterKey = parseResult.lineAfterKey
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
    , lineAfterKey = parseResult.lineAfterKey
    }

view : Int -> Model -> List Highlight
view key model =
  ChordParser.view key model.chordModel ++
    List.map (Highlight "" "#008000" "#ffffff") model.comments ++
      List.map (Highlight "" "#ffffff" "#ff0000") model.indents ++
        List.concatMap (viewFlag model.key) model.flags

viewFlag : Int -> ParsedFlag -> List Highlight
viewFlag key flag =
  List.concat
    [ if flag.name.s == flag.cleanName then
        [ Highlight "" "#0000ff" "#ffffff" flag.name ]
      else
        []
    , if flag.value.s == flag.cleanValue then
        case flag.flag of
          Just innerFlag ->
            [ Highlight
                ""
                ( if innerFlag == KeyFlag key then "#c00000"
                  else "#a0a0a0"
                )
                "#ffffff"
                flag.value
            ]
          Nothing ->
            []
      else
        []
    ]

getChords : Model -> List (List (Maybe IdChord))
getChords = ChordParser.getChords << .chordModel

getSuggestions :
  Int -> Model -> ( List Suggestion, Dict String (Set ( Int, Int )) )
getSuggestions key model =
  let
    flagSuggestions =
      Suggestion.groupByReplacement
        (List.filterMap getSuggestion model.flags)
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
insertUnion k x y d =
  Dict.insert k (Set.union x y) d

getSuggestion : ParsedFlag -> Maybe ( List Swatch, Substring )
getSuggestion flag =
  case
    ( flag.name.s /= flag.cleanName, flag.value.s /= flag.cleanValue )
  of
    ( False, False ) -> Nothing
    ( True, False ) ->
      Just
        ( [ Swatch "#0000ff" "#ffffff" flag.cleanName ]
        , flag.name
        )
    ( False, True ) ->
      Just
        ( [ { fg = if flag.flag == Nothing then "#000000" else "#c00000"
            , bg = "#ffffff"
            , s = flag.cleanValue
            }
          ]
        , flag.value
        )
    ( True, True ) ->
      Just
        ( if String.startsWith "#" flag.cleanValue then
            [ Swatch "#0000ff" "#ffffff" flag.cleanName
            , Swatch "#000000" "#ffffff" " "
            , Swatch "#008000" "#ffffff" flag.cleanValue
            ]
          else if flag.flag == Nothing then
            [ Swatch "#0000ff" "#ffffff" flag.cleanName
            , Swatch "#000000" "#ffffff" (" " ++ flag.cleanValue)
            ]
          else
            [ Swatch "#0000ff" "#ffffff" flag.cleanName
            , Swatch "#000000" "#ffffff" " "
            , Swatch "#c00000" "#ffffff" flag.cleanValue
            ]
        , flag.code
        )

type alias ParseResult =
  { words : List Substring
  , flags : List ParsedFlag
  , comments : List Substring
  , indents : List Substring
  , key : Int
  , lineAfterKey : Int
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
        lastTrueAndBeyond (nameHighlighted << .flag) lineResults
      of
        [] -> lineResults
        _ :: beyond -> beyond
  in let
    flags = List.filterMap .flag lineResults
  in let
    ( key, lineAfterKey ) =
      let keys = List.filterMap getFlagKey flags in
        case List.reverse keys of
          lastKey :: _ -> lastKey
          [] -> ( 0, 0 ) -- C major is the default
  in
    { words = List.concatMap .words chordArea
    , flags = flags
    , comments = List.filterMap .comment lineResults
    , indents = List.filterMap .indent lineResults
    , key = key
    , lineAfterKey = lineAfterKey
    }

nameHighlighted : Maybe ParsedFlag -> Bool
nameHighlighted maybeFlag =
  case maybeFlag of
    Nothing -> False
    Just flag -> flag.name.s == flag.cleanName

lastTrueAndBeyond : (a -> Bool) -> List a -> List a
lastTrueAndBeyond pred xs =
  case xs of
    [] -> []
    x :: rest ->
      case lastTrueAndBeyond pred rest of
        [] -> if pred x then xs else []
        result -> result

getFlagKey : ParsedFlag -> Maybe ( Int, Int )
getFlagKey flag =
  case flag.flag of
    Nothing -> Nothing
    Just innerFlag ->
      case innerFlag of
        KeyFlag key -> Just ( key, flag.nextLineStart )

type alias LineResult =
  { words : List Substring
  , flag : Maybe ParsedFlag
  , comment : Maybe Substring
  , indent : Maybe Substring
  }

type alias ParsedFlag =
  { flag : Maybe Flag
  , name : Substring
  , cleanName : String
  , value : Substring
  , cleanValue : String
  , code : Substring
  , nextLineStart : Int
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
            let flag = parseFlag code in
              { words =
                  if nameHighlighted flag then []
                  else Substring.find All (Regex.regex "[^ ]+") code
              , flag = flag
              , comment = comment
              , indent = Nothing
              }

parseFlag : Substring -> Maybe ParsedFlag
parseFlag code =
  case
    Substring.find (AtMost 1) (Regex.regex "^[a-zA-Z]+ *: *") code
  of
    [] ->
      Nothing
    nameAndSpace :: _ ->
      let
        name =
          { nameAndSpace
          | s = String.trimRight nameAndSpace.s
          }
      in let
        cleanName =
          ( String.toLower
              (String.trimRight (String.dropRight 1 name.s))
          ) ++ ":"
      in let
        parser =
          case cleanName of
            "key:" -> Just Flag.parseKey
            _ -> Nothing
      in
        case parser of
          Nothing ->
            Nothing
          Just parseValue ->
            let
              value =
                Substring.dropLeft (String.length nameAndSpace.s) code
            in let
              flag = parseValue value.s
            in let
              missingSpace =
                value.s /= "" &&
                  String.length nameAndSpace.s == String.length name.s
            in
              Just
                { flag = flag
                , name =
                    if missingSpace then { name | s = "" } else name
                , cleanName = cleanName
                , value =
                    if missingSpace then { value | s = "" } else value
                , cleanValue =
                    case flag of
                      Nothing -> value.s
                      Just x -> Flag.codeValue x
                , code = code
                , nextLineStart = 0
                }
