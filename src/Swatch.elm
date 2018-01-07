module Swatch exposing (Swatch, concat, view)

import Html exposing (Html, mark, text)
import Html.Attributes exposing (style)

type alias Swatch =
  { fg : String
  , bg : String
  , s : String
  }

concat : List Swatch -> String
concat = String.concat << List.map .s

view : Swatch -> Html msg
view swatch =
  case ( swatch.fg, swatch.bg ) of
    ( "#000000", "#ffffff" ) ->
      text swatch.s
    ( "#000000", _ ) ->
      mark
        [ style
            [ ( "color", "inherit" )
            , ( "background", swatch.bg )
            , ( "border-radius", "3px" )
            ]
        ]
        [ text swatch.s ]
    ( _, "#ffffff" ) ->
      mark
        [ style
            [ ( "color", swatch.fg )
            , ( "background", "transparent" )
            , ( "text-shadow", "0px 0px 0px white" )
            , ( "position", "relative" )
            , ( "z-index", "1" )
            , ( "pointer-events", "none" )
            ]
        ]
        [ text swatch.s ]
    _ ->
      mark
        [ style
            [ ( "color", "inherit" )
            , ( "background", swatch.bg )
            , ( "border-radius", "3px" )
            ]
        ]
        [ mark
            [ style
                [ ( "color", swatch.fg )
                , ( "background", "transparent" )
                , ( "text-shadow", "0px 0px 0px " ++ swatch.bg )
                , ( "position", "relative" )
                , ( "z-index", "1" )
                , ( "pointer-events", "none" )
                ]
            ]
            [ text swatch.s ]
        ]
