module Song exposing (Song, view)

import Colour
import CustomEvents exposing (onLeftDown, onKeyDown)
import Name
import PlayStatus exposing (PlayStatus, IdChord)

import Html exposing (Html, button, div, span)
import Html.Attributes as Attributes exposing (style)

type alias Song = List (List (Maybe IdChord))

view : Int -> PlayStatus -> List (List (Maybe IdChord)) -> Html PlayStatus.Msg
view key playStatus song =
  div
    [ style
        [ ( "font-size", "18pt" )
        , ( "margin-right", "5px" )
        , ( "margin-bottom", "5px" )
        ]
    ]
    (List.map (viewLine key playStatus) song)

viewLine : Int -> PlayStatus -> List (Maybe IdChord) -> Html PlayStatus.Msg
viewLine key playStatus line =
  div
    [ style
        [ ( "display", "flex" ) ]
    ]
    (List.map (viewCell key playStatus) line)

viewCell : Int -> PlayStatus -> Maybe IdChord -> Html PlayStatus.Msg
viewCell key playStatus cell =
  case cell of
    Just idChord ->
      viewChord key playStatus idChord
    Nothing ->
      viewSpace

viewChord : Int -> PlayStatus -> IdChord -> Html PlayStatus.Msg
viewChord key playStatus { id, chord } =
  let
    selected =
      playStatus.active == id || playStatus.next == id
  in let
    stopButton = playStatus.active == id && playStatus.stoppable
  in let
    play =
      if stopButton then
        PlayStatus.Stop
      else
        PlayStatus.Play (IdChord id chord)
  in
    span
      [ style
          [ ( "border-style"
            , if playStatus.next == id then
                "dashed"
              else
                "solid"
            )
          , ( "flex", "none" )
          , ( "border-width", "5px" )
          , ( "margin-right", "-5px" )
          , ( "margin-bottom", "-5px" )
          , ( "border-color"
            , if selected then
                "#3399ff"
              else
                "transparent"
            )
          , ( "border-radius", "10px" )
          ]
      ]
      [ button
          [ onLeftDown play
          , onKeyDown
              [ ( 13, play )
              , ( 32, play )
              ]
          , Attributes.id (toString id)
          , style
              [ ( "background", Colour.bg key chord )
              , ( "color", Colour.fg chord )
              , ( "font", "inherit" )
              , ( "width", "75px" )
              , ( "height", "75px" )
              , ( "padding", "0px 3px" )
              , ( "border"
                , String.concat
                    [ "1px solid rgba(0, 0, 0, "
                    , Colour.borderOpacity chord
                    , ")"
                    ]
                )
              , ( "border-radius", "5px" )
              , ( "box-shadow"
                , String.concat
                    [ "inset 18px 34px 20px -20px rgba(255, 255, 255, "
                    , Colour.shineOpacity chord
                    , ")"
                    ]
                )
              , ( "cursor", "pointer" )
              , ( "white-space", "nowrap" )
              ]
          ]
          ( if stopButton then
              [ span
                  [ style
                     [ ( "background", Colour.fg chord )
                     , ( "width", "20px" )
                     , ( "height", "20px" )
                     , ( "display", "inline-block" )
                     , ( "vertical-align", "middle" )
                     ]
                  ]
                  []
              ]
            else
              Name.view chord
          )
      ]

viewSpace : Html msg
viewSpace =
  span
    [ style
        [ ( "width", "80px" )
        , ( "height", "80px" )
        , ( "flex", "none" )
        ]
    ]
    []
