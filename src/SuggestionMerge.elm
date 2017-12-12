module SuggestionMerge exposing (mergeSuggestions)

import Substring exposing (Substring)
import Suggestion exposing (Suggestion)

import Dict exposing (Dict)

mergeSuggestions :
  (a -> Maybe String) -> (a -> Suggestion) -> (a -> Substring) -> List a ->
    List Suggestion
mergeSuggestions getReplacement getSuggestion getRange xs =
  Dict.values
    ( List.foldl
        (addSuggestion getReplacement getSuggestion getRange)
        Dict.empty
        xs
    )

addSuggestion :
  (a -> Maybe String) -> (a -> Suggestion) -> (a -> Substring) -> a ->
    Dict String Suggestion -> Dict String Suggestion
addSuggestion getReplacement getSuggestion getRange x suggestions =
  case getReplacement x of
    Nothing ->
      suggestions
    Just replacement ->
      Dict.update
        replacement
        (updateSuggestion replacement getSuggestion getRange x)
        suggestions

updateSuggestion :
  String -> (a -> Suggestion) -> (a -> Substring) -> a ->
    Maybe Suggestion -> Maybe Suggestion
updateSuggestion replacement getSuggestion getRange x maybeSuggestion =
  Just
    ( case maybeSuggestion of
        Nothing ->
          let suggestion = getSuggestion x in
            { suggestion | replacement = replacement }
        Just suggestion ->
          { suggestion | ranges = getRange x :: suggestion.ranges }
    )
