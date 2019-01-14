module ChordView exposing (view)

import Chord exposing (Chord)
import Click exposing (Click)
import Colour
import CustomEvents exposing (isAudioTimeButton, onClickWithAudioTime)
import IdChord exposing (IdChord)
import Name
import Selection exposing (Selection)

import Html exposing (Html, span, button)
import Html.Attributes as Attributes exposing (style, class, classList)
import Html.Lazy

view : Int -> Selection -> IdChord -> List (Html Click)
view tonic selection idChord =
  let
    member = Selection.member idChord.id selection
    scheduled = Selection.scheduled idChord.id selection
    playing = not (Selection.sequenceFinished selection)
  in
    [ Html.Lazy.lazy2
        viewBorder
        scheduled
        (scheduled && not member)
    , if playing then
        Html.Lazy.lazy3
          viewButton
          tonic
          member
          idChord
      else
        Html.Lazy.lazy3
          viewSelectButton
          tonic
          member
          idChord
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
            "#00cd00"
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

viewButton : Int -> Bool -> IdChord -> Html Click
viewButton tonic stoppable idChord =
  button
    [ isAudioTimeButton True
    , onClickWithAudioTime (Click idChord)
    , class "chordButton"
    , style "width" "3.2em"
    , style "height" "3.2em"
    , style "background" (Colour.bg tonic idChord.chord)
    , style "color" (Colour.fg idChord.chord)
    , style "font" "inherit" -- somehow this redundant style changes
                             -- line-height and keeps text with
                             -- superscripts from sagging
    , style "padding" "0"
    , style
        "border"
        ( String.concat
            [ "1px solid rgba(0, 0, 0, "
            , Colour.borderOpacity idChord.chord
            , ")"
            ]
        )
    , style "border-radius" "5px"
    , style
        "box-shadow"
        ( String.concat
            [ "inset 0.75em 1.42em 0.83em -0.83em rgba(255, 255, 255, "
            , Colour.shineOpacity idChord.chord
            , ")"
            ]
        )
    , style "cursor" "pointer"
    , style "white-space" "nowrap"
    ]
    ( if stoppable then
        [ span
            [ style "background" (Colour.fg idChord.chord)
            , style "width" "1em"
            , style "height" "1em"
            , style "display" "inline-block"
            , style "vertical-align" "middle"
            ]
            []
        ]
      else
        Name.view idChord.chord
    )

viewSelectButton : Int -> Bool -> IdChord -> Html Click
viewSelectButton tonic selected idChord =
  span
    [ style "display" "inline-block"
    , style
        "background"
        ( if selected then
            "#3399ff"
          else
            "transparent"
        )
    ]
    [ button
        [ isAudioTimeButton True
        , onClickWithAudioTime (Selection.Clicked << Tuple.pair idChord)
        , class "chordButton"
        , style "width" "2.8em"
        , style "height" "2.8em"
        , style "background" (Colour.bg tonic idChord.chord)
        , style "color" (Colour.fg idChord.chord)
        , style "font" "inherit" -- somehow this redundant style changes
                                -- line-height and keeps text with
                                -- superscripts from sagging
        , style
            "border"
            ( String.concat
                [ "1px solid rgba(0, 0, 0, "
                , Colour.borderOpacity idChord.chord
                , ")"
                ]
            )
        , style "padding" "0"
        , style "margin" "0.2em"
        , style "white-space" "nowrap"
        ]
        (Name.view idChord.chord)
    ]
