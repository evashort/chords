module Flag exposing (Flag, parse, Rule, rule, Codex, highlights, suggestions)

import Highlight exposing (Highlight)
import Suggestion exposing (Suggestion)
import Swatch exposing (Swatch)

import Dict exposing (Dict)
import Regex exposing (Regex, HowMany(..))

type alias Flag a =
  { name : String
  , fromString : String -> Maybe a
  , default : a
  , code : a -> String
  }

parse : Flag a -> List Substring -> a
parse flag lines =
  parseHelp flag (List.reverse lines)

parseHelp : Flag a -> List Substring -> a
parseHelp flag lines =
  case lines of
    [] -> flag.default
    line :: rest ->
      case parseLine flag line of
        Nothing ->
          parseHelp flag rest
        Just value ->
          value

parseLine : Flag a -> Substring -> Maybe a
parseLine flag line =
  if not (String.startsWith (flag.name ++ ": ") line.s) then
    Nothing
  else
    let
      valueString =
        String.dropLeft (String.length flag.name + 2) line.s
    in
      if String.startsWith " " valueString then
        Nothing
      else
        case flag.fromString valueString of
          Nothing ->
            Nothing
          Just value ->
            if flag.code value == valueString then
              Just value
            else
              Nothing

type alias Rule = (String, String -> Maybe String)

rule : Flag -> Rule
rule flag =
  ( flag.name, Maybe.map flag.code << flag.fromString )

type alias Codex = Dict String (String -> Maybe String)

highlights : Codex -> List Substring -> List Highlight
highlights codex lines =
  highlightsHelp codex Dict.empty (List.reverse lines)

highlightsHelp :
  Codex -> Dict String String -> List Substring -> List Highlight
highlightsHelp codex values lines =
  case lines of
    [] ->
      []
    line :: rest ->
      let (highlights, newValues) = lineHighlights codex values line in
        highlights ++ highlightsHelp codex newValues rest

lineHighlights :
  Codex -> Dict String String -> Substring ->
    (List Highlight, Dict String String)
lineHighlights codex values line =
  case submatches highlightRegex line.s of
    [ Just key, Just value ] ->
      case Dict.get key codex of
        Nothing ->
          ( [], values )
        Just valueFixer ->
          if String.startsWith " " value then
            ( [ keyHighlight key line ], values )
          else
            case Dict.get key values of
              Nothing ->
                if valueFixer value == Just value then
                  ( [ keyHighlight key line, valueHighlight value line ]
                  , Dict.insert key value values
                  )
                else
                  ( [ keyHighlight key line ], values )
              Just effectiveValue ->
                if effectiveValue == Just value then
                  ( [ keyHighlight key line, valueHighlight value line ]
                  , values
                  )
                else if valueFixer value == Just value then
                  ( [ keyHighlight key line, overridden value line ]
                  , values
                  )
                else
                  ( [ keyHighlight key line ], values )
    _ ->
      if String.endsWith ":" line.s then
        if Dict.member (String.dropRight 1 line.s) codex then
          ( [ Highlight "#0000ff" "#ffffff" line ], values )
        else
          ( [], values )

highlightRegex : Regex
highlightRegex = Regex.regex "^([^:]+): (.+)"

keyHighlight : String -> Substring -> Highlight
keyHighlight key line =
  Highlight "#0000ff" "#ffffff" (Substring.left (String.length key + 1) line)

valueHighlight : String -> Substring -> Highlight
valueHighlight value line =
  Highlight "#c00000" "#ffffff" (Substring.right (String.length value) line)

overridden : String -> Substring -> Highlight
overridden value line =
  Highlight "#a0a0a0" "#ffffff" (Substring.right (String.length value) line)

suggestions : Codex -> List Substring -> List Suggestion
suggestions codex lines =
  Suggestion.groupByReplacement
    (List.filterMap (lineSuggestion codex) lines)

lineSuggestion : Codex -> Substring -> Maybe (List Swatch, Substring)
lineSuggestion codex line =
  case submatches suggestionRegex line.s of
    [ Just key, Just afterKey, Just beforeValue, Just value ] ->
      let lowerKey = String.lower key in
        case Dict.get lowerKey codex of
          Nothing ->
            Nothing
          Just valueFixer ->
            if value == "" then
              if key == lowerKey && afterKey == ":" then
                Nothing
              else
                Just ( [ keySwatch lowerKey ], line )
            else
              case valueFixer value of
                Nothing ->
                  if beforeValue /= " " then
                    Just
                      ( [ keySwatch lowerKey, textSwatch (" " ++ value) ]
                      , line
                      )
                  else if key == lowerKey && afterKey == ":" then
                    Nothing
                  else
                    Just
                      ( [ keySwatch lowerKey ]
                      , Substring.left
                          (String.length key + String.length afterKey)
                          line
                      )
                Just fixedValue ->
                  case
                    ( key == lowerKey
                    , afterKey
                    , beforeValue
                    , value == fixedValue
                    )
                  of
                    ( True, ":", " ", True ) ->
                      Nothing
                    ( True, ":", " ", False ) ->
                      Just
                        ( [ valueSwatch fixedValue ]
                        , Substring.right (String.length value) line
                        )
                    ( _, _, " ", True ) ->
                      Just
                        ( [ keySwatch lowerKey ]
                        , Substring.left
                            (String.length key + String.length afterKey)
                            line
                        )
                    _ ->
                      Just
                        ( [ keySwatch lowerKey
                          , textSwatch " "
                          , valueSwatch fixedValue
                          ]
                        , line
                        )
    _ ->
      Nothing

keySwatch : String -> Swatch
keySwatch key =
  Swatch "#0000ff" "#ffffff" (key ++ ":")

valueSwatch : String -> Swatch
valueSwatch = Swatch "#c00000" "#ffffff"

textSwatch : String -> Swatch
textSwatch value = Swatch "#000000" "#ffffff"

suggestionRegex : Regex
suggestionRegex = Regex.regex "^([^:]*[^: ])( *:)( *)(.*)"

submatches : Regex -> String -> List (Maybe String)
submatches regex s =
  List.concatMap .submatches (Regex.find (AtMost 1) regex s)
