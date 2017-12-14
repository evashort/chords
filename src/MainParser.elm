module MainParser exposing
  (Model, init, update, view, getChords, getSuggestions)

import ChordParser exposing (IdChord)
import Highlight exposing (Highlight)
import Substring exposing (Substring)
import Suggestion exposing (Suggestion)
import SuggestionMerge
import Swatch exposing (Swatch)

import Regex exposing (Regex, HowMany(..), Match)
import Set exposing (Set)

type alias Model =
  { chordModel : ChordParser.Model
  , flags : List ParsedFlag
  , comments : List Substring
  , indents : List Substring
  }

init : Int -> Substring -> Model
init firstId whole =
  let parseResult = parse whole in
    { chordModel =
        ChordParser.init firstId parseResult.words
    , flags = parseResult.flags
    , comments = parseResult.comments
    , indents = parseResult.indents
    }

update : Substring -> Model -> Model
update whole model =
  let parseResult = parse whole in
    { chordModel =
        ChordParser.update parseResult.words model.chordModel
    , flags = parseResult.flags
    , comments = parseResult.comments
    , indents = parseResult.indents
    }

view : Int -> Model -> List Highlight
view key model =
  ChordParser.view key model.chordModel ++
    List.map (Highlight "" "#008000" "#ffffff") model.comments ++
      List.map (Highlight "" "#ffffff" "#ff0000") model.indents ++
        List.concatMap viewFlag model.flags

viewFlag : ParsedFlag -> List Highlight
viewFlag flag =
  if
    flag.name.s /= flag.cleanName ||
      (flag.value.s /= "" && not (hasSpace flag))
  then
    []
  else
    [ Highlight "" "#0000ff" "#ffffff" flag.name ]

getChords : Model -> List (List (Maybe IdChord))
getChords = ChordParser.getChords << .chordModel

getSuggestions : Model -> List Suggestion
getSuggestions model =
  List.sortBy
    (.i << .firstRange)
    ( List.concat
        [ ChordParser.getSuggestions model.chordModel
        , SuggestionMerge.mergeSuggestions
            flagReplacement
            flagSuggestion
            flagRange
            model.flags
        ]
    )

flagReplacement : ParsedFlag -> Maybe String
flagReplacement flag =
  if flag.value.s /= "" && not (hasSpace flag) then
    Just (flag.cleanName ++ " " ++ flag.value.s)
  else if flag.name.s /= flag.cleanName then
    Just flag.cleanName
  else
    Nothing

flagSuggestion : ParsedFlag -> Suggestion
flagSuggestion flag =
  if flag.value.s /= "" && not (hasSpace flag) then
    { replacement = ""
    , swatchLists =
        let
          swatchList =
            if String.startsWith "#" flag.value.s then
              [ Swatch "#0000ff" "#ffffff" flag.cleanName
              , Swatch "#000000" "#ffffff" " "
              , Swatch "#008000" "#ffffff" flag.value.s
              ]
            else
              [ Swatch "#0000ff" "#ffffff" flag.cleanName
              , Swatch "#000000" "#ffffff" (" " ++ flag.value.s)
              ]
        in
          ( swatchList, swatchList, swatchList )
    , firstRange = flag.code
    , ranges = []
    }
  else
    { replacement = ""
    , swatchLists =
        let
          swatchList =
            [ Swatch "#0000ff" "#ffffff" flag.cleanName ]
        in
          ( swatchList, swatchList, swatchList )
    , firstRange = flag.name
    , ranges = []
    }

flagRange : ParsedFlag -> Substring
flagRange flag =
  if flag.value.s /= "" && not (hasSpace flag) then
    flag.code
  else
    flag.name

type alias ParseResult =
  { words : List Substring
  , flags : List ParsedFlag
  , comments : List Substring
  , indents : List Substring
  }

parse : Substring -> ParseResult
parse whole =
  let
    lineResults =
      List.map
        parseLine
        (Substring.find All (Regex.regex ".*\n?") whole)
  in let
    ( flagArea, chordArea ) =
      case splitAfterLastTrue (nameHighlighted << .flag) lineResults of
        Nothing -> ( [], lineResults )
        Just x -> x
  in
    { words = List.concatMap .words chordArea
    , flags = List.filterMap .flag lineResults
    , comments = List.filterMap .comment lineResults
    , indents = List.filterMap .indent lineResults
    }

nameHighlighted : Maybe ParsedFlag -> Bool
nameHighlighted maybeFlag =
  case maybeFlag of
    Nothing ->
      False
    Just flag ->
      flag.name.s == flag.cleanName &&
        (flag.value.s == "" || hasSpace flag)

hasSpace : ParsedFlag -> Bool
hasSpace flag =
  String.length flag.code.s >
    String.length flag.name.s + String.length flag.value.s

splitAfterLastTrue : (a -> Bool) -> List a -> Maybe ( List a, List a )
splitAfterLastTrue pred xs =
  case xs of
    [] -> Nothing
    x :: rest ->
      case splitAfterLastTrue pred rest of
        Just ( before, after ) -> Just ( x :: before, after )
        Nothing ->
          if pred x then Just ( [ x ], rest )
          else Nothing

type alias LineResult =
  { words : List Substring
  , flag : Maybe ParsedFlag
  , comment : Maybe Substring
  , indent : Maybe Substring
  }

type alias ParsedFlag =
  { name : Substring
  , cleanName : String
  , value : Substring
  , code : Substring
  , nextLineStart : Int
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
            Just c -> Substring.before c.i line
            Nothing -> line
      in let
        code =
          case
            Substring.find (AtMost 1) (Regex.regex "^.*[^ \n]") codeAndSpace
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
            let
              nextLineStart =
                if String.endsWith "\n" line.s then
                  Substring.stop line
                else
                  0
            in let
              flag = parseFlag nextLineStart code
            in
              { words =
                  if nameHighlighted flag then
                    []
                  else
                    let
                      normalWords =
                        Substring.find All (Regex.regex "[^ \n]+") code
                    in
                      if normalWords == [] then
                        []
                      else
                        normalWords ++
                          Substring.find (AtMost 1) (Regex.regex "\n$") line
              , flag = flag
              , comment = comment
              , indent = Nothing
              }

parseFlag : Int -> Substring -> Maybe ParsedFlag
parseFlag nextLineStart code =
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
      in
        if Set.member cleanName flagNames then
          Just
            { name = name
            , cleanName = cleanName
            , value =
                Substring.dropLeft (String.length nameAndSpace.s) code
            , code = code
            , nextLineStart = nextLineStart
            }
        else
          Nothing

flagNames : Set String
flagNames = Set.fromList [ "key:" ]
