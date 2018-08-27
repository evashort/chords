module Swatch exposing (Swatch, plain, concat, view)

import Html exposing (Html, mark, text)
import Html.Attributes exposing (style)

type alias Swatch =
  { fg : String
  , bg : String
  , s : String
  }

plain : String -> Swatch
plain s =
  Swatch "#000000" "#ffffff" s

concat : List Swatch -> String
concat = String.concat << List.map .s

view : Swatch -> Html msg
view swatch =
  case ( swatch.fg, swatch.bg ) of
    ( "#000000", "#ffffff" ) ->
      text swatch.s
    ( "#000000", _ ) ->
      mark
        [ style "color" "inherit"
        , style "background" swatch.bg
        , style "border-radius" "3px"
        ]
        [ text swatch.s ]
    ( _, "#ffffff" ) ->
      mark
        [ style "color" swatch.fg
        , style "background" "transparent"
        , style "text-shadow" "0px 0px 0px white"
        , style "position" "relative"
        , style "z-index" "1"
        , style "pointer-events" "none"
        ]
        [ text swatch.s ]
    _ ->
      mark
        [ style "color" "inherit"
        , style "background" swatch.bg
        , style "border-radius" "3px"
        ]
        [ mark
            [ style "color" swatch.fg
            , style "background" "transparent"
            , style "text-shadow" ("0px 0px 0px " ++ swatch.bg)
            , style "position" "relative"
            , style "z-index" "1"
            , style "pointer-events" "none"
            ]
            [ text swatch.s ]
        ]
