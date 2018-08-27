module Rulebook exposing (Rulebook, init, getHighlights, getSuggestions)

import Rule exposing (Fixer, Rule)
import Highlight exposing (Highlight)
import Submatches exposing (submatches)
import Substring exposing (Substring)
import Suggestion exposing (Suggestion)
import Swatch exposing (Swatch)

import Dict exposing (Dict)
import Regex exposing (Regex)

type alias Rulebook = Dict String Fixer

init : List Rule -> Rulebook
init = Dict.fromList

getHighlights : Rulebook -> List Substring -> List Highlight
getHighlights book lines =
  getHighlightsHelp book Dict.empty (List.reverse lines)

getHighlightsHelp :
  Rulebook -> Dict String String -> List Substring -> List Highlight
getHighlightsHelp book values lines =
  case lines of
    [] ->
      []
    line :: rest ->
      let (highlights, newValues) = lineHighlights book values line in
        highlights ++ getHighlightsHelp book newValues rest

lineHighlights :
  Rulebook -> Dict String String -> Substring ->
    (List Highlight, Dict String String)
lineHighlights book values line =
  case submatches highlightRegex line.s of
    [ Just key, Nothing, maybeValue ] ->
      case ( Dict.get key book, maybeValue ) of
        ( Nothing, _ ) ->
          ( [], values )
        ( _, Nothing ) ->
          ( [ keyHighlight key line ], values )
        ( Just fix, Just value ) ->
          case Dict.get key values of
            Nothing ->
              if fix value == Just value then
                ( [ keyHighlight key line, valueHighlight value line ]
                , Dict.insert key value values
                )
              else
                ( [ keyHighlight key line ], values )
            Just effectiveValue ->
              if effectiveValue == value then
                ( [ keyHighlight key line, valueHighlight value line ]
                , values
                )
              else if fix value == Just value then
                ( [ keyHighlight key line, overridden value line ]
                , values
                )
              else
                ( [ keyHighlight key line ], values )
    _ ->
      ( [], values )

highlightRegex : Regex
highlightRegex =
  Maybe.withDefault
    Regex.never
    (Regex.fromString "([^:]+):([^ ])? ?([^ ].*)?")

keyHighlight : String -> Substring -> Highlight
keyHighlight key line =
  Highlight "#0000ff" "#ffffff" (Substring.left (String.length key + 1) line)

valueHighlight : String -> Substring -> Highlight
valueHighlight value line =
  Highlight "#c00000" "#ffffff" (Substring.right (String.length value) line)

overridden : String -> Substring -> Highlight
overridden value line =
  Highlight "#a0a0a0" "#ffffff" (Substring.right (String.length value) line)

getSuggestions : Rulebook -> List Substring -> List Suggestion
getSuggestions book lines =
  Suggestion.groupByReplacement
    (List.filterMap (lineSuggestion book) lines)

lineSuggestion : Rulebook -> Substring -> Maybe (List Swatch, Substring)
lineSuggestion book line =
  case submatches suggestionRegex line.s of
    [ Just key, Just afterKey, maybeBeforeValue, maybeValue ] ->
      let
        lowerKey = String.toLower key
        beforeValue = Maybe.withDefault "" maybeBeforeValue
        value = Maybe.withDefault "" maybeValue
      in
        case Dict.get lowerKey book of
          Nothing ->
            Nothing
          Just fix ->
            if value == "" then
              if key == lowerKey && afterKey == ":" then
                Nothing
              else
                Just ( [ keySwatch lowerKey ], line )
            else
              case fix value of
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
                    , ( afterKey, beforeValue )
                    , value == fixedValue
                    )
                  of
                    ( True, ( ":", " " ), True ) ->
                      Nothing
                    ( True, ( ":", " " ), False ) ->
                      Just
                        ( [ valueSwatch fixedValue ]
                        , Substring.right (String.length value) line
                        )
                    ( _, ( _, " " ), True ) ->
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

suggestionRegex : Regex
suggestionRegex =
  Maybe.withDefault
    Regex.never
    (Regex.fromString "^([^:]*[^: ])( *:)( *)(.*)")

keySwatch : String -> Swatch
keySwatch key =
  Swatch "#0000ff" "#ffffff" (key ++ ":")

valueSwatch : String -> Swatch
valueSwatch = Swatch "#c00000" "#ffffff"

textSwatch : String -> Swatch
textSwatch = Swatch "#000000" "#ffffff"
