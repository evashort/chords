module IdChord exposing (IdChord, Msg(..), fromChord, count, view)

import Chord exposing (Chord)
import Colour
import CustomEvents exposing (isAudioTimeButton, onClickWithAudioTime)
import Name
import PlayStatus exposing (PlayStatus)

import Dict exposing (Dict)
import Html exposing (Html, span, button)
import Html.Attributes as Attributes exposing (style, class, classList)

type alias IdChord =
  { id : Int
  , chord : Chord
  }

type Msg
  = Play (IdChord, Float)
  | Stop Float

fromChord : Chord -> Maybe IdChord
fromChord chord =
  case Dict.get chord.flavor schemes of
    Nothing ->
      Nothing
    Just i ->
      Just (IdChord (12 * i + chord.root) chord)

count : Int
count = 12 * Dict.size schemes

schemes : Dict (List Int) Int
schemes =
  Dict.fromList
    (List.indexedMap reversedTuple Chord.flavors)

reversedTuple : a -> b -> ( b, a )
reversedTuple x y =
  ( y, x )

view : Int -> PlayStatus -> IdChord -> List (Html Msg)
view tonic playStatus { id, chord } =
  [ span
    [ classList
        [ ( "chordBorder", True )
        , ( "hasBorder"
          , PlayStatus.hasBorder playStatus id
          )
        , ( "hasDashedBorder"
          , PlayStatus.hasDashedBorder playStatus id
          )
        ]
    , style
        [ ( "position", "absolute" )
        , ( "top", "-5px" )
        , ( "left", "-5px" )
        , ( "right", "-5px" )
        , ( "bottom", "-5px" )
        , ( "pointer-events", "none" )
        , ( "border-width", "5px" )
        , ( "border-radius", "10px" )
        , ( "border-color"
          , if PlayStatus.hasBorder playStatus id then
              "#3399ff"
            else
              "transparent"
          )
        , ( "border-style"
          , if PlayStatus.hasDashedBorder playStatus id then
              "dashed"
            else
              "solid"
          )
        ]
    ]
    []
  , let
      action =
        if PlayStatus.hasStopButton playStatus id then
          Stop
        else
          Play << (,) (IdChord id chord)
    in
      button
        [ isAudioTimeButton True
        , onClickWithAudioTime action
        , class "chordButton"
        , style
            [ ( "width", "100%" )
            , ( "height", "100%" )
            , ( "background", Colour.bg tonic chord )
            , ( "color", Colour.fg chord )
            , ( "font", "inherit" ) -- somehow this redundant style changes
                                    -- line-height and keeps text with
                                    -- superscripts from sagging
            , ( "padding", "0" )
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
        ( if PlayStatus.hasStopButton playStatus id then
            [ span
                [ style
                   [ ( "background", Colour.fg chord )
                   , ( "width", "1em" )
                   , ( "height", "1em" )
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
