module Suggestion exposing (Suggestion, view, unique)

import Html exposing (Html, mark, text)
import Html.Attributes exposing (style)
import Set exposing (Set)

type alias Suggestion =
  { s : String
  , fg : String
  , bg : String
  }

view : Suggestion -> Html msg
view suggestion =
  mark
    [ style
        [ ( "color", suggestion.fg )
        , ( "background", suggestion.bg )
        , ( "border-radius", "3px" )
        ]
    ]
    [ text suggestion.s ]

unique : List Suggestion -> List Suggestion
unique suggestions =
  uniqueHelp Set.empty suggestions

uniqueHelp : Set String -> List Suggestion -> List Suggestion
uniqueHelp seen suggestions =
  case suggestions of
    [] ->
      []
    suggestion :: rest ->
      if Set.member suggestion.s seen then
        uniqueHelp seen rest
      else
        suggestion :: uniqueHelp (Set.insert suggestion.s seen) rest
