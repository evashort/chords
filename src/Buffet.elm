module Buffet exposing
  (Buffet, Msg(..), LensChange, init, update, changeLenses, highlights, view)

import Highlight exposing (Highlight)
import Suggestion exposing (Suggestion)
import Swatch

import Html exposing (Html, div)
import Html.Attributes exposing (style)

type alias Buffet =
  { suggestions : List Suggestion
  , lenses : List Lens
  }

type alias Lens =
  { hover : Bool
  , index : Int
  }

type Msg
  = LensesChanged LensChange
  | Replace Suggestion

type LensChange
  = AddLens Lens
  | RemoveLens Bool

init : List Suggestion -> Buffet
init suggestions =
  { suggestions = suggestions
  , lenses = []
  }

update : List Suggestion -> Buffet -> Buffet
update suggestions buffet =
  let
    sameReplacement =
      countSharedReplacements suggestions buffet.suggestions
  in let
    samePosition =
      min (sameReplacement + 1) (List.length suggestions)
  in
    { suggestions = suggestions
    , lenses =
        List.filter
          (indexLessThan samePosition (List.length suggestions))
          buffet.lenses
    }

countSharedReplacements : List Suggestion -> List Suggestion -> Int
countSharedReplacements xs ys =
  case ( xs, ys ) of
    ( x :: xRest, y :: yRest ) ->
      if Swatch.concat x.swatches == Swatch.concat y.swatches then
        1 + countSharedReplacements xRest yRest
      else
        0
    _ ->
      0

indexLessThan : Int -> Int -> Lens -> Bool
indexLessThan hoverCount focusCount lens =
  lens.index < (if lens.hover then hoverCount else focusCount)

changeLenses : LensChange -> Buffet -> Buffet
changeLenses lensChange buffet =
  case lensChange of
    AddLens lens ->
      { buffet
      | lenses =
          lens :: List.filter ((/=) lens.hover << .hover) buffet.lenses
      }
    RemoveLens hover ->
      { buffet
      | lenses = List.filter ((/=) hover << .hover) buffet.lenses
      }

highlights : Buffet -> List Highlight
highlights buffet =
  case buffet.lenses of
    [] ->
      []
    lens :: _ ->
      case List.drop lens.index buffet.suggestions of
        [] ->
          []
        suggestion :: _ ->
          List.map
            (Highlight "#ffffff" "#aaaaaa")
            suggestion.ranges

view : Buffet -> Html Msg
view buffet =
  div
    [ style
        [ ( "grid-area", "buffet" )
        ]
    ]
    (List.indexedMap viewSuggestion buffet.suggestions)

viewSuggestion : Int -> Suggestion -> Html Msg
viewSuggestion i suggestion =
  Html.map
    (interpretSuggestionMessage i suggestion)
    (Suggestion.view suggestion.swatches)

interpretSuggestionMessage : Int -> Suggestion -> Suggestion.Msg -> Msg
interpretSuggestionMessage i suggestion suggestionMessage =
  case suggestionMessage of
    Suggestion.Hover True ->
      LensesChanged (AddLens { hover = True, index = i })
    Suggestion.Hover False ->
      LensesChanged (RemoveLens True)
    Suggestion.Focus True ->
      LensesChanged (AddLens { hover = False, index = i })
    Suggestion.Focus False ->
      LensesChanged (RemoveLens False)
    Suggestion.Replace ->
      Replace suggestion
