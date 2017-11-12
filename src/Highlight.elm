module Highlight exposing (Highlight, viewWhole, view, suggestDeletion)

import Substring exposing (Substring)

import Html exposing (Html)
import Html.Attributes exposing (style)

type alias Highlight =
  { fg : String
  , bg : String
  , substring : Substring
  }

viewWhole : Substring -> List Highlight -> List (Html msg)
viewWhole whole highlights =
  let
    sorted = List.sortBy (.i << .substring) highlights
  in let
    before =
      case sorted of
        [] -> whole
        x :: rest -> Substring.before x.substring.i whole
  in
    case before.s of
      "" -> viewWholeHelp whole sorted
      _ -> Html.text before.s :: viewWholeHelp whole sorted

viewWholeHelp : Substring -> List Highlight -> List (Html msg)
viewWholeHelp whole highlights =
  case highlights of
    [] ->
      []
    x :: rest ->
      let
        xView = view x
      in let
        between =
          case rest of
            [] ->
              Substring.after (Substring.stop x.substring) whole
            y :: _ ->
              Substring.between
                (Substring.stop x.substring)
                y.substring.i
                whole
      in
        case between.s of
          "" -> xView :: viewWholeHelp whole rest
          _ -> xView :: Html.text between.s :: viewWholeHelp whole rest

view : Highlight -> Html msg
view highlight =
  case ( highlight.fg, highlight.bg ) of
    ( "#000000", "#ffffff" ) ->
      Html.text highlight.substring.s
    ( "#000000", _ ) ->
      Html.mark
        [ style
            [ ( "color", "inherit" )
            , ( "background", highlight.bg )
            , ( "border-radius", "3px" )
            ]
        ]
        [ Html.text highlight.substring.s ]
    ( _, "#ffffff" ) ->
      Html.mark
        [ style
            [ ( "color", highlight.fg )
            , ( "background", "transparent" )
            , ( "text-shadow", "0px 0px 0px white" )
            , ( "position", "relative" )
            , ( "pointer-events", "none" )
            ]
        ]
        [ Html.text highlight.substring.s ]
    _ ->
      Html.mark
        [ style
            [ ( "color", "inherit" )
            , ( "background", highlight.bg )
            , ( "border-radius", "3px" )
            ]
        ]
        [ Html.mark
            [ style
                [ ( "color", highlight.fg )
                , ( "background", "transparent" )
                , ( "text-shadow", "0px 0px 0px " ++ highlight.bg )
                , ( "position", "relative" )
                , ( "pointer-events", "none" )
                ]
            ]
            [ Html.text highlight.substring.s ]
        ]

suggestDeletion : Substring -> Highlight
suggestDeletion = Highlight "#ffffff" "#ff0000"
