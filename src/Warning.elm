module Warning exposing (view)

import Html exposing (Html, span)
import Html.Attributes exposing (id, style, property)
import Json.Encode as Encode

view : Bool -> Html msg
view shouldWarn =
  span
    [ id "warning"
    , style [ ( "display", "none" ) ]
    , property "shouldWarn" (Encode.bool shouldWarn)
    ]
    []
