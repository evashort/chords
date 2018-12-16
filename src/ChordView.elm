module ChordView exposing (Msg(..), view)

import Chord exposing (Chord)
import Colour
import CustomEvents exposing (isAudioTimeButton, onClickWithAudioTime)
import IdChord exposing (IdChord)
import Name
import Player exposing (Player)

import Html exposing (Html, span, button)
import Html.Attributes as Attributes exposing (style, class, classList)
import Html.Lazy

type Msg
  = Play (IdChord, Float)
  | Stop Float

view : Int -> Player -> Maybe IdChord -> IdChord -> List (Html Msg)
view tonic player selection idChord =
  let
    stoppable = Player.stoppable player &&
      Player.active player == Just idChord
  in
    List.map
      ( Html.map
          ( if stoppable then
              Stop
            else
              Play << Tuple.pair idChord
          )
      )
      [ Html.Lazy.lazy2
          viewBorder
          (Player.active player == Just idChord || selection == Just idChord)
          (Player.next player == Just idChord.id)
      , Html.Lazy.lazy3
          viewButton
          tonic
          stoppable
          idChord.chord
      ]

viewBorder : Bool -> Bool -> Html msg
viewBorder solid dashed =
  span
    [ classList
        [ ( "chordBorder", True )
        , ( "hasBorder", solid || dashed )
        , ( "hasDashedBorder", dashed )
        ]
    , style "position" "absolute"
    , style "top" "-5px"
    , style "left" "-5px"
    , style "right" "-5px"
    , style "bottom" "-5px"
    , style "pointer-events" "none"
    , style "border-width" "5px"
    , style "border-radius" "10px"
    , style
        "border-color"
        ( if solid || dashed then
            "#3399ff"
          else
            "transparent"
        )
    , style
        "border-style"
        ( if dashed then
            "dashed"
          else
            "solid"
        )
    ]
    []

viewButton : Int -> Bool -> Chord -> Html Float
viewButton tonic stoppable chord =
  button
    [ isAudioTimeButton True
    , onClickWithAudioTime identity
    , class "chordButton"
    , style "width" "100%"
    , style "height" "100%"
    , style "background" (Colour.bg tonic chord)
    , style "color" (Colour.fg chord)
    , style "font" "inherit" -- somehow this redundant style changes
                             -- line-height and keeps text with
                             -- superscripts from sagging
    , style "padding" "0"
    , style
        "border"
        ( String.concat
            [ "1px solid rgba(0, 0, 0, "
            , Colour.borderOpacity chord
            , ")"
            ]
        )
    , style "border-radius" "5px"
    , style
        "box-shadow"
        ( String.concat
            [ "inset 18px 34px 20px -20px rgba(255, 255, 255, "
            , Colour.shineOpacity chord
            , ")"
            ]
        )
    , style "cursor" "pointer"
    , style "white-space" "nowrap"
    ]
    ( if stoppable then
        [ span
            [ style "background" (Colour.fg chord)
            , style "width" "1em"
            , style "height" "1em"
            , style "display" "inline-block"
            , style "vertical-align" "middle"
            ]
            []
        ]
      else
        Name.view chord
    )
