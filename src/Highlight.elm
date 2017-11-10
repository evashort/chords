module Highlight exposing
  (Highlight, fromSubstring, view, suggestDeletion, viewString)

import Substring exposing (Substring)

import Html exposing (Html)
import Html.Attributes exposing (style)

type alias Highlight =
  { fg : String
  , bg : String
  , start : Int
  , stop : Int
  }

fromSubstring : String -> String -> Substring -> Highlight
fromSubstring fg bg substring =
  { fg = fg
  , bg = bg
  , start = substring.i
  , stop = Substring.stop substring
  }

view : String -> List Highlight -> List (Html msg)
view string highlights =
  let
    sorted = List.sortBy .start highlights
  in let
    before =
      case sorted of
        [] -> string
        x :: rest -> String.left x.start string
  in
    case before of
      "" -> viewHelp string sorted
      _ -> Html.text before :: viewHelp string sorted

viewHelp : String -> List Highlight -> List (Html msg)
viewHelp string highlights =
  case highlights of
    [] ->
      []
    x :: rest ->
      let
        xView = viewString x.fg x.bg (String.slice x.start x.stop string)
      in let
        between =
          case rest of
            [] -> String.dropLeft x.stop string
            y :: _ -> String.slice x.stop y.start string
      in
        case between of
          "" -> xView :: viewHelp string rest
          _ -> xView :: Html.text between :: viewHelp string rest

viewString : String -> String -> String -> Html msg
viewString fg bg string =
  case ( fg, bg ) of
    ( "#000000", "#ffffff" ) ->
      Html.text string
    ( "#000000", _ ) ->
      Html.mark
        [ style
            [ ( "color", "inherit" )
            , ( "background", bg )
            , ( "border-radius", "3px" )
            ]
        ]
        [ Html.text string ]
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
        [ Html.text string ]
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
            [ Html.text string ]
        ]

suggestDeletion : Substring -> Highlight
suggestDeletion = fromSubstring "#ffffff" "#ff0000"
