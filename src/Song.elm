module Song exposing (Song, view)

import Colour
import CustomEvents exposing (onLeftDown, onKeyDown)
import Name
import PlayStatus exposing (PlayStatus, IdChord)

import Html exposing (Html, button, div, span)
import Html.Attributes as Attributes exposing (style)

type alias Song = List (List (Maybe IdChord))

view : String -> Int -> PlayStatus -> Song -> Html PlayStatus.Msg
view gridArea key playStatus song =
  span
    [ style
        [ ( "grid-area", gridArea )
        , ( "display", "grid" )
        , ( "position", "relative" )
        , ( "grid-auto-rows", "75px" )
        , ( "grid-auto-columns", "75px" )
        , ( "grid-row-gap", "5px" )
        , ( "grid-column-gap", "5px" )
        , ( "font-size", "18pt" )
        ]
    ]
    ( List.concatMap
        List.concat
        (indexedMap2d (viewCell key playStatus) song)
    )

indexedMap2d : (Int -> Int -> a -> b) -> List (List a) -> List (List b)
indexedMap2d f rows =
  List.indexedMap (List.indexedMap << f) rows

viewCell :
  Int -> PlayStatus -> Int -> Int -> Maybe IdChord ->
    List (Html PlayStatus.Msg)
viewCell key playStatus y x cell =
  Maybe.withDefault
    []
    (Maybe.map (viewChord key playStatus y x) cell)

viewChord :
  Int -> PlayStatus -> Int -> Int -> IdChord -> List (Html PlayStatus.Msg)
viewChord key playStatus y x { id, chord } =
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
          , if playStatus.active == id || playStatus.next == id then
              "#3399ff"
            else
              "transparent"
          )
        , ( "border-style"
          , if playStatus.next == id then
              "dashed"
            else
              "solid"
          )
        ]
    ]
    []
  , let
      stopButton =
        playStatus.active == id && playStatus.stoppable
    in let
      action =
        if stopButton then
          PlayStatus.Stop
        else
          PlayStatus.Play (IdChord id chord)
    in
      button
        [ onLeftDown action
        , onKeyDown
            [ ( 13, action )
            , ( 32, action )
            ]
        , Attributes.id (toString id)
        , style
            [ ( "grid-row", toString (y + 1) )
            , ( "grid-column", toString (x + 1) )
            , ( "background", Colour.bg key chord )
            , ( "color", Colour.fg chord )
            , ( "font", "inherit" )
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
