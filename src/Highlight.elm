module Highlight exposing (Highlight, fromText, group, view)

import Html exposing (Html)
import Html.Attributes exposing (style)

type alias Highlight =
  { fg : String
  , bg : String
  , text : String
  }

fromText : String -> Highlight
fromText text =
  { fg =
      case text of
        "Bo" -> "#ffffff"
        "keyword" -> "#8055ff"
        _ -> "#000000"
  , bg =
      case text of
        "C" -> "#f8facd"
        "Dm" -> "#eccdfa"
        "Em" -> "#d2facd"
        "F" -> "#facdcd"
        "G" -> "#c9ffff"
        "Am" -> "#ffe7c9"
        "Bo" -> "#005e93"
        _ -> "#ffffff"
  , text = text
  }

group : List Highlight -> List Highlight
group =
  List.foldr appendOrMerge []

appendOrMerge : Highlight -> List Highlight -> List Highlight
appendOrMerge x ys =
  case ys of
    y :: rest ->
      if x.fg == y.fg && x.bg == y.bg then
        { x | text = x.text ++ y.text } :: rest
      else
        x :: ys
    [] ->
      [ x ]

view : Highlight -> Html msg
view { fg, bg, text } =
  case ( fg, bg ) of
    ( "#000000", "#ffffff" ) ->
      Html.text text
    ( "#000000", _ ) ->
      Html.mark
        [ style
            [ ( "color", "inherit" )
            , ( "background", bg )
            , ( "border-radius", "3px" )
            ]
        ]
        [ Html.text text ]
    ( _, "#ffffff" ) ->
      Html.mark
        [ style
            [ ( "color", fg )
            , ( "background", "transparent" )
            , ( "text-shadow", "0px 0px 0px white" )
            , ( "position", "relative" )
            , ( "pointer-events", "none" )
            ]
        ]
        [ Html.text text ]
    _ ->
      Html.mark
        [ style
            [ ( "color", "inherit" )
            , ( "background", bg )
            , ( "border-radius", "3px" )
            ]
        ]
        [ Html.mark
            [ style
                [ ( "color", fg )
                , ( "background", "transparent" )
                , ( "text-shadow", "0px 0px 0px " ++ bg )
                , ( "position", "relative" )
                , ( "pointer-events", "none" )
                ]
            ]
            [ Html.text text ]
        ]
