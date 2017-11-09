module Suggestion exposing (Suggestion, view)

import Html exposing (Html, mark, text)
import Html.Attributes exposing (style)

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
