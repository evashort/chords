module Bracket exposing (view)

import Unit exposing (Unit, px, ch, fr, zero)

import Html exposing (Html, span)
import Html.Attributes exposing (style)

view : String -> Unit -> Unit -> Unit -> Unit -> Int -> Int -> Html msg
view gridArea top bottom sliderPadding bracketPadding center lowestNote =
  let
    boundaries =
      List.filter
        (isBoundary center lowestNote)
        (List.range (center - 7) (center + 7))
  in let
    ranges =
      List.map2 (,) boundaries (List.drop 1 boundaries)
  in let
    widths =
      List.map (width bracketPadding center) ranges
  in
    span
      [ style
          [ ( "grid-area", gridArea )
          , ( "display", "grid" )
          , ( "grid-template-areas"
            , String.concat
                [ "\""
                , String.join " " (List.indexedMap areas widths)
                , "\""
                ]
            )
          , ( "grid-template-columns"
            , String.join " " (List.map columns widths)
            )
          , ( "position", "absolute" )
          , ( "top", Unit.code top )
          , ( "bottom", Unit.code bottom )
          , ( "left"
            , (Unit.code << Unit.sum)
                [ sliderPadding, Unit.negative bracketPadding ]
            )
          , ( "right"
            , (Unit.code << Unit.sum)
                [ sliderPadding, Unit.negative bracketPadding ]
            )
          , ( "pointer-events", "none" )
          ]
      ]
      ( List.concat
          (List.indexedMap (nodes center lowestNote) ranges)
      )

isBoundary : Int -> Int -> Int -> Bool
isBoundary center lowestNote pitch =
  (pitch + 1) % 12 < 2 ||
    pitch == center - 7 ||
    pitch == center + 7 ||
    pitch == lowestNote ||
    pitch == lowestNote + 11

width : Unit -> Int -> (Int, Int) -> Unit
width padding center ( start, end ) =
  let
    count = end - start
  in let
    strictCount =
      min end (center + 6) - max start (center - 6)
  in
    Unit.sum
      [ if start % 12 == 0 then px 1.5 else zero
      , if end % 12 == 11 then px 1.5 else zero
      , if start % 12 == 11 then
          if count > strictCount then px (-1.5) else zero
        else
          px (3 * toFloat strictCount)
      , fr (toFloat strictCount)
      , if count > strictCount then padding else zero
      ]

areas : Int -> Unit -> String
areas i unit =
  if unit.fr == 0 || { unit | fr = 0 } == zero then
    "a" ++ toString i
  else
    "a" ++ toString i ++ " a" ++ toString i

columns : Unit -> String
columns unit =
  if unit.fr == 0 then
    Unit.code unit
  else
    let rest = { unit | fr = 0 } in
      if rest == zero then
        Unit.code unit
      else
        Unit.code rest ++ " " ++ toString unit.fr ++ "fr"

nodes : Int -> Int -> Int -> (Int, Int) -> List (Html msg)
nodes center lowestNote i ( start, end ) =
  if start % 12 == 11 then
    []
  else
    let
      boldStart = start >= lowestNote && start <= lowestNote + 11
    in let
      boldEnd = end >= lowestNote && end <= lowestNote + 11
    in
      [ span
          [ style
              [ ( "grid-area", "a" ++ toString i )
              , ( "margin-left"
                , if not boldStart && start % 12 == 0 then "1px"
                  else "0"
                )
              , ( "border-left"
                , if start % 12 == 0 then
                    if boldStart then "3px solid" else "1px solid"
                  else
                    "none"
                )
              , ( "border-top"
                , if boldStart && boldEnd then "3px solid"
                  else "1px solid"
                )
              , ( "border-right"
                , if end % 12 == 11 then
                    if boldEnd then "3px solid" else "1px solid"
                  else
                    "none"
                )
              , ( "margin-right"
                , if not boldEnd && end % 12 == 11 then "1px"
                  else "0"
                )
              ]
          ]
          []
      ]
