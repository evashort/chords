module IdChord exposing (IdChord, Msg(..), fromChord, count, view)

import Chord exposing (Chord)
import Colour
import CustomEvents exposing (onLeftDown, onKeyDown)
import Name
import PlayStatus exposing (PlayStatus)

import Dict exposing (Dict)
import Html exposing (Html, span, button)
import Html.Attributes as Attributes exposing (style)

type alias IdChord =
  { id : Int
  , chord : Chord
  }

type Msg
  = Play IdChord
  | Stop

fromChord : Chord -> IdChord
fromChord chord =
  case Dict.get chord.flavor schemes of
    Nothing ->
      Debug.crash
        ("IdChord.fromChord: Unknown flavor: " ++ toString chord.flavor)
    Just i ->
      IdChord (12 * i + chord.root) chord

count : Int
count = 12 * Dict.size schemes

schemes : Dict (List Int) Int
schemes =
  Dict.fromList
    ( List.indexedMap
        reversedTuple
        (Dict.values Chord.flavors)
    )

reversedTuple : a -> b -> ( b, a )
reversedTuple x y =
  ( y, x )

view : Int -> PlayStatus -> Int -> Int -> IdChord -> List (Html Msg)
view tonic playStatus y x { id, chord } =
  [ span
    [ style
        [ ( "grid-row-start", toString (y + 1) )
        , ( "grid-column-start", toString (x + 1) )
        , ( "grid-row-end", "span 1" )
        , ( "grid-column-end", "span 1" )
        , ( "position", "absolute" )
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
          Play (IdChord id chord)
    in
      button
        [ onLeftDown action
        , onKeyDown
            [ ( 13, action )
            , ( 32, action )
            ]
        , style
            [ ( "grid-row", toString (y + 1) )
            , ( "grid-column", toString (x + 1) )
            , ( "align-self", "stretch" )
            , ( "justify-self", "stretch" )
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
